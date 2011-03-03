open Subcommand
open Guppy_cmdobjs

let escape = Base.sqlite_escape
module TIAMR = AlgMap.AlgMapR (Tax_id.OrderedTaxId)

(* if rank is less than the tax rank of ti, then move up the taxonomy until
 * the first time that the tax rank is less than or equal to rank *)
let classify_at_rank td rank ti =
  let rec aux curr_ti =
    if rank >= Tax_taxonomy.get_tax_rank td curr_ti then curr_ti
    else
      aux
        (try Tax_taxonomy.get_ancestor td curr_ti with
        | Tax_taxonomy.NoAncestor _ -> assert(false))
  in
  aux ti

(* apply f to all of the keys and add the results together *)
let keymap_add_by f m =
  List.fold_right
    (fun (k,v) -> (TIAMR.add_by (f k) v))
    (Tax_id.TaxIdMapFuns.to_pairs m)
    TIAMR.M.empty

(* m is a taxid_algmap and this outputs a list of string_arrays, one for each
 * placement *)
let classif_stral td name desired_rank m =
  List.map
    (fun (ti, p) ->
      [|
        name;
        Tax_taxonomy.get_rank_name td desired_rank;
        Tax_taxonomy.rank_name_of_tax_id td ti;
        Tax_id.to_string ti;
        Printf.sprintf "%g" p;
      |])
    (Tax_id.TaxIdMapFuns.to_pairs m)

let classify how criterion n_ranks td pr f =
  try
    List.iter
      (fun pq ->
        let outl = ref [] in
        let m = ref
          (List.fold_right
             (fun p ->
               TIAMR.add_by
                 (how p)
                 (criterion p))
             (Pquery.place_list pq)
             (TIAMR.M.empty))
        in
        for desired_rank=(n_ranks-1) downto 0 do
          m := keymap_add_by (classify_at_rank td desired_rank) !m;
          outl :=
            (List.flatten
               (List.map
                  (fun name -> classif_stral td name desired_rank !m)
                  (Pquery.namel pq)))
          @ (!outl);
        done;
        f (!outl))
      (Placerun.get_pqueries pr)
  with
    | Placement.No_classif ->
      invalid_arg
        ((Placerun.get_name pr)^" contains unclassified queries!")


(* UI-related *)

class cmd () =
object (self)
  inherit subcommand () as super
  inherit refpkg_cmd ~required:true as super_refpkg
  inherit placefile_cmd () as super_placefile

  val use_pp = flag "--pp"
    (Plain (false, "Use posterior probability for our criteria."))
  val csv_out = flag "--csv"
    (Plain (false, "Write .class.csv files containing CSV data."))
  val sqlite_out = flag "--sqlite"
    (Plain (false, "Write .class.sqlite files containing sqlite insert statements."))

  method specl =
    super_refpkg#specl
  @ [
    toggle_flag use_pp;
    toggle_flag csv_out;
    toggle_flag sqlite_out;
  ]

  method desc =
    "outputs classification information in a tabular format"
  method usage = "usage: classify [options] placefile[s]"

  method private placefile_action prl =
    let rp = self#get_rp in
    let criterion = if (fv use_pp) then Placement.post_prob else Placement.ml_ratio in
    let td = Refpkg.get_taxonomy rp in
    let n_ranks = Tax_taxonomy.get_n_ranks td in
    let out_func pr =
      if fv csv_out then
        (fun outl ->
          let prn = Placerun.get_name pr in
          let ch = open_out (prn ^ ".class.csv") in
          output_string ch "name,desired_rank,rank,tax_id,likelihood,origin\n";
          List.iter
            (fun arr -> Printf.fprintf ch "%s,%s\n" (String.concat "," (Array.to_list arr)) prn)
            outl;
          close_out ch)
      else if fv sqlite_out then
        (fun outl ->
          let prn = Placerun.get_name pr in
          let ch = open_out (prn ^ ".class.sqlite") in
          List.iter
            (fun arr -> Printf.fprintf ch
              "INSERT INTO placements VALUES (%s, %s);\n"
              (String.concat ", " (List.map escape (Array.to_list arr)))
              (escape prn))
            outl;
          close_out ch)
      else
        (fun outl ->
          let ch = open_out ((Placerun.get_name pr)^".class.tab") in
          String_matrix.write_padded ch (Array.of_list outl);
          close_out ch)
    in
    List.iter
      (fun pr ->
        classify Placement.contain_classif criterion n_ranks td pr (out_func pr))
      prl

end

