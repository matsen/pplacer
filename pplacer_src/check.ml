(* routines for doing sanity checks *)

open MapsSets
open Fam_batteries

exception Duplicate_name of string

(* run through the query sequences and make sure everything looks OK *)
let pretend model ref_align query_fnames =
  let len = Alignment.length ref_align in
  Printf.printf "found %d reference sequences of length %d.\n"
    (Alignment.n_seqs ref_align) len;
  let base_map =
    match Model.seq_type model with
    | Alignment.Nucleotide_seq ->
        print_endline "nucleotide sequences"; Nuc_models.nuc_map
    | Alignment.Protein_seq ->
        print_endline "amino acid sequences"; Prot_models.prot_map
  in
  List.iter
    (fun fname ->
      let (size,_) =
        List.fold_left
          (fun (i,s) (name,seq) ->
            String.iter
              (fun c ->
                try
                  let _ =
                    CharMap.find (Char.uppercase c) base_map in ()
                with
                | Not_found ->
                    failwith
                      (Printf.sprintf
                        "%c is not a known base in %s" c name))
              seq;
            if StringSet.mem name s then
              raise (Duplicate_name name)
            else if len <> String.length seq then
              failwith (name^" does not have the same length as the reference alignment!")
            else (i+1,StringSet.add name s))
          (0,StringSet.empty)
          (Fasta.of_file fname)
      in
      Printf.printf "%s: %d sequences.\n" fname size)
    query_fnames

(* check that dir_name is actually a directory *)
let directory dir_name =
  try
    if not (Sys.is_directory dir_name) then
      raise (Sys_error "")
  with
  | Sys_error _ ->
      failwith
        (Printf.sprintf "Bad directory specification: '%s'"
        dir_name)
