(* The Naive Bayes taxonomic classifier of Wang et. al. *)

open Ppatteries

module BA = Bigarray
module BA1 = BA.Array1
module BA2 = BA.Array2

(* We bootstrap on columns rather than on words of a read. Because of this, the
 * number of actual words used in a given bootstrap sample for a read (the
 * number of "occupied" words) will often deviate from the expected number for
 * that read. In order to make sure that these have the right number of occupied
 * words, we throw out the bootstrap sample if the number of occupied words is
 * not within `boot_range_pct` of the expected number.
 *
 * For this reason, our bootstrap matrix has `n_boot` times `extra_boot_factor`
 * rows. The bootstrap function will not provde a reasonable value if we throw
 * out `(extra_boot_factor-1)*n_boot` values; it is not hard to see using the
 * CDF of the negative binomal distribution that the probability of this
 * happening for the values below is essentially zero. *)
let extra_boot_factor = 5
let boot_range_pct = 15
let in_boot_range expected actual =
  abs (actual - expected) <= expected * boot_range_pct / 100

let bases = "ACGT"

let informative = function
  | 'A' | 'C' | 'G' | 'T' -> true
  | _ -> false

let word_uninformative s =
  String.enum s |> Enum.for_all informative |> not

exception Invalid_base of char

(* get an index of a base *)
let base_idx base =
  try
    String.of_char base |> String.find bases
  with Not_found -> raise (Invalid_base base)

(* convert from a word to an int. Successive base indices are packed into the
 * integer using left shifts. *)
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

(* generalized count something by words in a sequence. generalized because we
 * are able to specify a modify function specifying action. *)
let gen_count_by_seq word_length modify seq =
  0 -- (String.length seq - word_length)
  |> Enum.map (flip (String.sub seq) word_length)
  |> Enum.iter (junction word_uninformative (const ()) (word_to_int |- modify))

let rank_tax_map_of_refpkg rp =
  let td = Refpkg.get_taxonomy rp in
  Refpkg.get_seqinfom rp
  |> StringMap.enum
  |> Enum.map (Tuple.Tuple2.map2 (fun {Tax_seqinfo.tax_id} -> tax_id))
  |> Convex.gen_build_rank_tax_map StringMap.empty StringMap.add td some

(* The preclassifier accumulates reference sequences before classification. When
 * we add sequences to a preclassifier we update the various counts, which will
 * allow us to generate the priors as well as allocate the correct size of
 * taxid_word_counts matrix in the classifier.  *)
let random_winner_max_index vec =
  0 --^ BA1.dim vec
  |> flip Enum.fold None (fun accum idx ->
    let x = vec.{idx} in
    match accum with
    | Some (_, prev_x, _) when x < prev_x -> accum
    | Some (prev_winner, prev_x, count) when x = prev_x ->
      let count' = succ count in
      let winner = if Random.int count' = 0 then idx else prev_winner in
      Some (winner, prev_x, count')
    | _ -> Some (idx, x, 1))
  |> Option.get
  |> Tuple3.first

(* the thing that accumulates reference sequences before classification *)
module Preclassifier = struct
  type base = {
    word_length: int;
    n_words: int;
    tax_ids: Tax_id.t array;
  }
  type 'a t = {
    base: base;
    freq_table: (int, 'a, BA.c_layout) BA2.t;
    taxid_counts: int array;
    word_counts: int array;
    seq_count: int ref;
  }

  exception Tax_id_not_found of Tax_id.t

  (* make a preclassifier, given kind, word_length, and tax_ids *)
  let make kind word_length tax_ids =
    if word_length > max_word_length then
      failwith
        (Printf.sprintf "max_word_length is %d (given word_length %d)"
           max_word_length word_length);
    let n_words = 1 lsl (word_length * 2)
    and n_taxids = Array.length tax_ids in
    let freq_table = BA2.create kind BA.c_layout n_taxids n_words
    and taxid_counts = Array.make n_taxids 0
    and word_counts = Array.make n_words 0
    and seq_count = ref 0 in
    BA2.fill freq_table 0;
    {base = {word_length; n_words; tax_ids}; word_counts; taxid_counts;
     freq_table; seq_count}

  (* find the index of a tax_id in the tax_ids array *)
  let tax_id_idx c tid =
    try
      Array.findi ((=) tid) c.base.tax_ids
    with Not_found -> raise (Tax_id_not_found tid)

  (* add a sequence to the counts for a particular tax_id *)
  let add_seq c tax_id seq =
    let i = tax_id_idx c tax_id
    and word_seen = Array.make c.base.n_words false in
    incr c.seq_count;
    c.taxid_counts.(i) <- c.taxid_counts.(i) + 1;
    gen_count_by_seq
      c.base.word_length
      (fun j -> word_seen.(j) <- true)
      seq;
    Array.iteri
      (fun j seen ->
        if seen then begin
          c.freq_table.{i, j} <- succ c.freq_table.{i, j};
          c.word_counts.(j) <- succ c.word_counts.(j)
        end)
      word_seen

  let to_taxid_word_counts c dest =
    let n = float_of_int (succ !(c.seq_count)) in
    let prior_counts = Array.init
      c.base.n_words
      (fun j ->
        (* w_j is the prior for seeing word j *)
        let w_j = float_of_int c.word_counts.(j) in
        (* (n(w_j) + 0.5) / (N + 1) *)
        (w_j +. 0.5) /. n)
    in
    BA2.modifyij
      (fun j i _ ->
        let m = c.freq_table.{i, j}
        and denom = log (float_of_int c.taxid_counts.(i) +. 1.) in
        (* log (m(w_j) + prior_counts[j]) - denom *)
        log (float_of_int m +. prior_counts.(j)) -. denom)
      dest

end

(* The classifier: the thing that actually holds the data for classification.
 * The most important component is taxid_word_counts, which is indexed on the
 * rows by the words and on the columns by the taxids.
*)
module Classifier = struct
  type t = {
    pc: Preclassifier.base;
    (* The matrix used for NBC classification. *)
    taxid_word_counts: Matrix.matrix;
    (* The matrix of randomly-generated bootstraps. *)
    boot_matrix: (int, BA.int16_unsigned_elt, BA.c_layout) BA2.t;
    (* A vector to hold the results of a classification multiplication. *)
    classify_vec: Gsl_vector.vector;
  }

  type rank = Rank of int | Auto_rank | All_ranks

  (* make a classifier from a preclassifier *)
  let make ?(n_boot = 100) ?map_file ?rng c =
    let open Preclassifier in
    let n_taxids = Array.length c.base.tax_ids
    and rng = Option.default (Random.get_state ()) rng
    and boot_rows = n_boot * extra_boot_factor in
    let taxid_word_counts, fill_counts = match map_file with
      | Some (fd, also_write) ->
        BA2.map_file
          fd
          BA.float64
          BA.c_layout
          also_write
          c.base.n_words
          n_taxids,
        also_write
      | None -> Matrix.create c.base.n_words n_taxids, true
    and fill_boot_row vec =
      Random.State.enum_int rng c.base.n_words
      (* boot with 1/word_length of the words. this number of bootstrapped
       * columns is assumed below in the definition of `expected` in the
       * bootstrap function. *)
        |> Enum.take (c.base.n_words / c.base.word_length)
        |> Enum.iter (fun i -> vec.{i} <- succ vec.{i})
    and boot_matrix = BA2.create
      BA.int16_unsigned
      BA.c_layout
      boot_rows
      c.base.n_words
    and classify_vec = Gsl_vector.create ~init:0. n_taxids in
    if fill_counts then
      Preclassifier.to_taxid_word_counts c taxid_word_counts;
    BA2.fill boot_matrix 0;
    0 --^ boot_rows
      |> Enum.iter (fun i -> BA2.slice_left boot_matrix i |> fill_boot_row);
    {pc = c.base; taxid_word_counts; boot_matrix; classify_vec}

  (* Find the tax_id associated with a count vector using the classifier cf. *)
  let classify_vec ?(random_tie_break = false) cf vec =
    let open Preclassifier in
    let dest = cf.classify_vec in
    Gsl_vector.set_zero dest;
    Linear.float_mat_int_vec_mul dest cf.taxid_word_counts vec;
    (if random_tie_break then random_winner_max_index dest
    else Gsl_vector.max_index dest)
      |> Array.get cf.pc.tax_ids

  (* fill a vector with counts for a sequence *)
  let count_seq cf ?(like_rdp = false) seq =
    let open Preclassifier in
    let vec = BA1.create BA.int16_unsigned BA.c_layout cf.pc.n_words in
    let counter_fn =
      if like_rdp then
        fun i -> vec.{i} <- 1
      else
        fun i -> vec.{i} <- succ vec.{i}
    in
    BA1.fill vec 0;
    gen_count_by_seq cf.pc.word_length counter_fn seq;
    vec

  (* classify a sequence, returning a tax_id *)
  let classify cf ?like_rdp ?random_tie_break seq =
    count_seq cf ?like_rdp seq |> classify_vec ?random_tie_break cf

  (* Bootstrap a sequence, returning a map from tax_ids to a float on the range
   * (0, 1] representing the percentage of bootstrappings done that produced a
   * particular tax_id. As you can read about at the top of this file, we do
   * bootstrapping a little differently than is traditional. We keep on
   * sampling resamplings (that are effectively just determined once) until we
   * get n_boot samples that are within the proper range. *)
  let bootstrap cf ?like_rdp ?random_tie_break seq =
    let open Preclassifier in
    let module TIM = Tax_id.TaxIdMap in
    let seq_word_counts = count_seq cf ?like_rdp seq in
    let boot_rows = BA2.dim1 cf.boot_matrix in
    let n_boot = boot_rows / extra_boot_factor in
    if n_boot = 0 then
      TIM.singleton (classify_vec ?random_tie_break cf seq_word_counts) 1.
    else (* ... *)
    let booted_word_counts = BA1.create
      BA.int16_unsigned
      BA.c_layout
      cf.pc.n_words
    (* the expected number of occupied columns under bootstrapping *)
    and expected = (Linear.int_vec_tot seq_word_counts) / cf.pc.word_length
    (* n_successes is the number of times we've gotten a re-sample that is
      * within the proper range. *)
    and n_successes = ref 0 in
    let counts = 0 --^ boot_rows
    |> Enum.fold
        (fun accum i ->
          if !n_successes >= n_boot then accum (* done *)
          else begin
            let boot_row = BA2.slice_left cf.boot_matrix i in
            Linear.int_vec_pairwise_prod
              booted_word_counts boot_row seq_word_counts;
            if in_boot_range expected (Linear.int_vec_tot booted_word_counts)
            then begin
              (* the number of occupied columns for this bootstrap resample is
               * within the desired range *)
              let ti = classify_vec ?random_tie_break cf booted_word_counts in
              incr n_successes;
              TIM.modify_def 0 ti succ accum
              end
            else accum
          end)
        TIM.empty
    in
    let total = TIM.values counts |> Enum.sum |> float_of_int in
    TIM.map (fun c -> float_of_int c /. total) counts

  let find_auto_rank td rank_tax_map =
    let n_all_seqs = IntMap.values rank_tax_map
      |> Enum.map StringMap.keys
      |> Enum.flatten
      |> StringSet.of_enum
      |> StringSet.cardinal
    and highest_rank, _ = IntMap.max_binding rank_tax_map in
    let rec is_viable rank_map =
      (StringMap.cardinal rank_map * 100 / n_all_seqs) > 80
    and aux rank =
      match IntMap.Exceptionless.find rank rank_tax_map with
      | _ when rank = -1 -> failwith "no rank could be automatically determined"
      | Some rank_map when is_viable rank_map -> rank
      | _ -> aux (rank - 1)
    in
    aux highest_rank
      |> tap
          (Tax_taxonomy.get_rank_name td
           |- dprintf "automatically determined best rank: %s\n")

  let _of_refpkg ?ref_aln ?n_boot ?map_file ?rng word_length rank_idx rp =
    let td = Refpkg.get_taxonomy rp
    and rank_tax_map = rank_tax_map_of_refpkg rp in
    let rank_idx =
      if rank_idx = -1 then find_auto_rank td rank_tax_map else rank_idx
    in
    let preclassif = IntMap.find rank_idx rank_tax_map
      |> StringMap.values
      |> Tax_id.TaxIdSet.of_enum
      |> Tax_id.TaxIdSet.enum
      |> Array.of_enum
      |> Preclassifier.make Bigarray.int word_length
    and rp_name = Refpkg.get_name rp in
    begin match map_file with
    | Some (_, false) ->
      dprintf "using stored reference counts for refpkg %s.\n" rp_name
    | _ ->
      dprintf "counting sequences from refpkg %s.\n" rp_name;
      (* a map from reference sequence names to chosen-rank tax_ids *)
      let seq_tax_ids = IntMap.find rank_idx rank_tax_map
      and filter m (k, seq) =
        match StringMap.Exceptionless.find k m with
        | Some v -> Some (v, Alignment.ungap seq)
        | None -> None
      in
      let ref_aln = Option.default (Refpkg.get_aln_fasta rp) ref_aln in
      let progress_fn =
        progress_displayer
          "counting reference sequence %s (%d/%d)..."
          (Array.length ref_aln)
      in
      Array.enum ref_aln
        |> Enum.map (tap (fst |- progress_fn))
        |> Enum.filter_map (filter seq_tax_ids)
        |> Enum.iter (uncurry (Preclassifier.add_seq preclassif))
    end;
    make ?map_file ?n_boot ?rng preclassif

  (* Take each sequence and put it in the classifier at every rank of its
   * lineage. *)
  let _all_ranks_of_refpkg ?ref_aln ?n_boot ?rng word_length rp =
    let td = Refpkg.get_taxonomy rp
    and seqinfo = Refpkg.get_seqinfom rp in
    let preclassif = Tax_id.TaxIdMap.keys td.Tax_taxonomy.tax_rank_map
      |> Array.of_enum
      |> Preclassifier.make Bigarray.int word_length
    in
    dprintf "counting sequences from refpkg %s.\n" (Refpkg.get_name rp);
    let ref_aln = Option.default (Refpkg.get_aln_fasta rp) ref_aln in
    let progress_fn =
      progress_displayer
        "counting reference sequence %s (%d/%d)..."
        (Array.length ref_aln)
    and expand_lineage (ti, seq) =
      Tax_taxonomy.get_lineage td ti
        |> List.enum
        |> Enum.map (fun ti' -> ti', seq)
    in
    Array.enum ref_aln
      |> Enum.map
          (tap (fst |- progress_fn)
           |- (Tax_seqinfo.tax_id_by_node_label seqinfo *** Alignment.ungap)
           |- expand_lineage)
      |> Enum.flatten
      |> Enum.iter (uncurry (Preclassifier.add_seq preclassif));
    make ?n_boot ?rng preclassif

  let of_refpkg ?ref_aln ?n_boot ?map_file ?rng word_length rank rp =
    let td = Refpkg.get_taxonomy rp
    and rank_tax_map = rank_tax_map_of_refpkg rp in
    match begin
      match rank with
      | Rank rank_idx -> Some rank_idx
      | Auto_rank -> Some (find_auto_rank td rank_tax_map)
      | All_ranks -> None
    end with
    | Some rank_idx ->
      _of_refpkg ?ref_aln ?n_boot ?map_file ?rng word_length rank_idx rp
    | None -> _all_ranks_of_refpkg ?ref_aln ?n_boot ?rng word_length rp

end
