(* pplacer v1.0. Copyright (C) 2009-2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer.  If not, see <http://www.gnu.org/licenses/>.
 *
 * routines for doing sanity checks
*)

open MapsSets
open Fam_batteries

(* run through the query sequences and make sure everything looks OK *)
let pretend model ref_align query_fnames =
  let len = Alignment.length ref_align in
  Printf.printf "found %d reference sequences of length %d.\n"
    (Alignment.n_seqs ref_align) len;
  let base_map = 
    match Model.seq_type model with
    | Alignment.Nucleotide_seq -> Nuc_models.nuc_map 
    | Alignment.Protein_seq -> Prot_models.prot_map
  in
  List.iter
    (fun fname -> 
      let ch = Fasta_channel.of_fname fname in
      let (size,_) =
        Fasta_channel.named_seq_fold
          (fun (name,seq) (i,s) -> 
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
              raise (Fasta_channel.Duplicate_name name)
            else if len <> String.length seq then
              failwith (name^" does not have the same length as the reference alignment!")
            else (i+1,StringSet.add name s))
          (0,StringSet.empty)
          ch
      in
      Printf.printf "%s: %d sequences.\n" fname size;
      Fasta_channel.close ch;)
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
