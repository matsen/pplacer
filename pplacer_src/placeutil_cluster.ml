(* pplacer v1.1. Copyright (C) 2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *)

type rounded_placement = 
  {
    location: int;
    ml_ratio: int;
    distal_bl: int;
    pendant_bl: int;
  }

let rounded_placement_of_placement multiplier p = 
  let round x = Base.round (multiplier *. x) in
  {
    location = p.Placement.location;
    ml_ratio = round p.Placement.ml_ratio;
    distal_bl = round p.Placement.distal_bl;
    pendant_bl = round p.Placement.pendant_bl;
  }

type rounded_pquery = rounded_placement list

(* we sort here so that we can compare *)
let rounded_pquery_of_pquery multiplier pq = 
  List.sort 
    compare
    (List.map 
      (rounded_placement_of_placement multiplier) 
      (pq.Pquery.place_list))

module RPQMap = 
  Map.Make
    (struct 
      type t = rounded_pquery
      let compare = Pervasives.compare
    end)

let add_listly k v m = 
  if RPQMap.mem k m then RPQMap.add k (v::(RPQMap.find k m)) m
  else RPQMap.add k [v] m

(* given a list of pqueries, we round them, then cluster the pqueries which are
 * identical after the rounding step. *)
let round_pquery_list sig_figs pql = 
  assert(sig_figs > 0);
  let multiplier = Base.int_pow 10. sig_figs 
  in
  (* collect placements together using a map *)
  let m =
    List.fold_left 
      (fun m pq -> add_listly (rounded_pquery_of_pquery multiplier pq) pq m)
      RPQMap.empty
      pql
  in
  (* take the first placement in the collected list, and give it all of the
   * names associated with the other pqueries *)
  List.map 
    (fun pql ->
      {(List.hd pql) with Pquery.namel = 
        List.flatten (List.map (fun pq -> pq.Pquery.namel) pql)})
    (RPQMap.fold (fun _ v l -> (List.rev v)::l) m []) (* the clustered ones *)

let round_placerun out_name sig_figs pr = 
  {pr with 
    Placerun.pqueries = round_pquery_list sig_figs (pr.Placerun.pqueries);
    name = out_name;}

(* UI-related *)

module Prefs = struct
  type prefs = 
    {
      out_prefix: string ref;
      sig_figs: int ref;
    }

  let out_prefix p = !(p.out_prefix)
  let sig_figs p = !(p.sig_figs)

  let defaults () =
    {
      out_prefix = ref "";
      sig_figs = ref 3;
    }

  let specl_of_prefs prefs = 
[
  "-o", Arg.Set_string prefs.out_prefix,
  "Set the filename to write to. Otherwise write to stdout.";
  "--sig-figs", Arg.Set_int prefs.sig_figs,
  "Set the number of significant figures used for rounding.";
]
end

let cluster_cmd prefs invocation fnamel = 
  let out_prefix = Prefs.out_prefix prefs in
  if out_prefix = "" then 
    invalid_arg "Please specify an output prefix with -o";
  List.iter
    (fun fname ->
      let pr = Placerun_io.of_file fname in
      let out_name = (out_prefix^(pr.Placerun.name)) in
      Placerun_io.to_file
        invocation
        (out_name^".place")
        (round_placerun out_name (Prefs.sig_figs prefs) pr))
    fnamel

let of_argl = function
  | [] -> print_endline "clusters the placements by rounding"
  | argl -> 
    let prefs = Prefs.defaults () in
    cluster_cmd
      prefs 
      (String.concat " " ("placeutil"::argl))
      (Subcommand.wrap_parse_argv
        argl
        (Prefs.specl_of_prefs prefs)
        "usage: cluster [options] placefile[s]")

