open Ppatteries

module BA = Bigarray
module BA2 = BA.Array2

let bases = "ACGT"

let informative = function
  | 'A' | 'C' | 'G' | 'T' -> true
  | _ -> false

let filter_informative = String.filter informative

exception Invalid_base of char

let base_idx base =
  try
    String.of_char base |> String.find bases
  with Not_found -> raise (Invalid_base base)

(* convert from a word to an int *)
let word_to_int =
  String.fold_left
    (fun accum c -> base_idx c |> (lor) (accum lsl 2))
    0

(* convert from an int to a word, with optional padding *)
let int_to_word ?word_length i =
  Enum.unfold
    i
    (function 0 -> None | x -> Some (bases.[x land 3], x lsr 2))
  |> (match word_length with
       | None -> identity
       | Some l -> flip Enum.append (Enum.repeat bases.[0]) |- Enum.take l)
  |> String.of_backwards

(* the max word length is (log_4 max_int) - 1 *)
let max_word_length =
  log (float_of_int max_int) /. log 4. |> int_of_float |> pred

let gen_count_by_seq word_length modify seq =
  0 -- (String.length seq - word_length)
  |> Enum.map (flip (String.sub seq) word_length)
  |> Enum.iter (word_to_int |- modify)

(* the thing that accumulates reference sequences before classification *)
module Preclassifier = struct
  type base = {
    word_length: int;
    n_words: int;
    tax_ids: Tax_id.t array;
  }
  type ('a, 'b) t = {
    base: base;
    freq_table: ('a, 'b, BA.c_layout) BA2.t;
    taxid_counts: int array;
    seq_count: int ref;
  }

  exception Tax_id_not_found of Tax_id.t

  let make kind word_length tax_ids =
    if word_length > max_word_length then
      failwith
        (Printf.sprintf "max_word_length is %d (given word_length %d)"
           max_word_length word_length);
    let n_words = 1 lsl (word_length * 2)
    and n_taxids = Array.length tax_ids in
    let freq_table = BA2.create kind BA.c_layout n_taxids n_words
    and taxid_counts = Array.make n_taxids 0
    and seq_count = ref 0 in
    {base = {word_length; n_words; tax_ids}; taxid_counts; freq_table; seq_count}

  let tax_id_idx c tid =
    try
      Array.findi ((=) tid) c.base.tax_ids
    with Not_found -> raise (Tax_id_not_found tid)

  let add_seq c succ tax_id seq =
    let i = tax_id_idx c tax_id in
    incr c.seq_count;
    c.taxid_counts.(i) <- c.taxid_counts.(i) + 1;
    gen_count_by_seq
      c.base.word_length
      (fun j -> BA2.get c.freq_table i j |> succ |> BA2.set c.freq_table i j)
      seq

end

(* the thing that does actual classification *)
module Classifier = struct
  type t = {
    pc: Preclassifier.base;
    taxid_word_counts: Gsl_matrix.matrix;
    boot_matrix: Gsl_matrix.matrix;
  }

  let make c ?(boot_rows = 100) add float_of_x =
    let open Preclassifier in
    let n_taxids = Array.length c.base.tax_ids
    and n = float_of_int (succ !(c.seq_count)) in
    let prior_counts = Array.init
      c.base.n_words
      (fun j ->
        let w_j = 0 --^ n_taxids
          |> Enum.map (fun i -> BA2.get c.freq_table i j)
          |> Enum.reduce add
          |> float_of_x
        in
        (* (n(w_j) + 0.5) / (N + 1) *)
        (w_j +. 0.5) /. n)
    in
    let taxid_word_counts = BA2.mapij
      (fun i j m ->
        let denom = log (float_of_int c.taxid_counts.(i) +. 1.) in
        (* log (m(w_j) + prior_counts[j]) - denom *)
        log (float_of_x m +. prior_counts.(j)) -. denom)
      BA.float64
      c.freq_table
    and fill_boot_row vec =
      Random.enum_int c.base.n_words
        |> Enum.take (c.base.n_words / c.base.word_length)
        |> Enum.iter
            (fun i -> Gsl_vector.get vec i +. 1. |> Gsl_vector.set vec i)
    and boot_matrix = Gsl_matrix.create boot_rows c.base.n_words in
    0 --^ boot_rows
      |> Enum.iter (fun i -> Gsl_matrix.row boot_matrix i |> fill_boot_row);
    {pc = c.base; taxid_word_counts; boot_matrix}

  let classify_vec cf vec =
    let open Preclassifier in
    Linear_utils.alloc_mat_vec_mul cf.taxid_word_counts vec
    |> Gsl_vector.max_index
    |> Array.get cf.pc.tax_ids

  let count_seq cf seq =
    let open Preclassifier in
    let vec = Gsl_vector.create cf.pc.n_words in
    gen_count_by_seq
      cf.pc.word_length
      (fun i -> Gsl_vector.get vec i +. 1. |> Gsl_vector.set vec i)
      seq;
    vec

  let classify cf seq =
    count_seq cf seq |> classify_vec cf

  let bootstrap cf seq =
    let open Preclassifier in
    let module TIM = Tax_id.TaxIdMap in
    let seq_word_counts = count_seq cf seq in
    let boot_rows, _ = Gsl_matrix.dims cf.boot_matrix in
    let incr = 1. /. float_of_int boot_rows |> (+.) in
    let booted_word_counts = Gsl_vector.create cf.pc.n_words in
    0 --^ boot_rows
    |> Enum.fold
        (fun accum i ->
          let boot_row = Gsl_matrix.row cf.boot_matrix i in
          Linear.vec_pairwise_prod booted_word_counts boot_row seq_word_counts;
          let ti = classify_vec cf booted_word_counts in
          TIM.modify_def 0. ti incr accum)
        TIM.empty

end
