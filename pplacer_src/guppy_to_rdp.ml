open Subcommand
open Guppy_cmdobjs

let index_of l x =
  let rec aux accum = function
    | x' :: rest ->
      if x = x' then accum
      else aux (accum + 1) rest
    | [] -> raise Not_found
  in aux 0 l

let gap_regexp = Str.regexp "-"
let taxid_regexp = Str.regexp "_"

let string_of_tax_id tax_id =
  Str.global_replace
    taxid_regexp
    "00"
    (Tax_id.to_string tax_id)

class cmd () =
object (self)
  inherit subcommand () as super
  inherit out_prefix_cmd () as super_out_prefix
  inherit refpkg_cmd ~required:true as super_refpkg

  val included_ranks = flag "-r"
    (Plain ([], "Ranks to include in the annotated fasta file. Can be comma-separated."))
  val list_ranks = flag "-l"
    (Plain (false, "List all of the ranks, by number."))

  method specl =
    super_out_prefix#specl
  @ super_refpkg#specl
  @ [
    string_list_flag included_ranks;
    toggle_flag list_ranks;
  ]

  method desc = "convert a reference package to a format RDP wants"
  method usage = "usage: to_rdp -c my.refpkg -o prefix"

  method action _ =
    if fv list_ranks then
      let rp = self#get_rp in
      let tax = Refpkg.get_taxonomy rp in
      Array.iteri
        (Printf.printf "%d: %s\n")
        tax.Tax_taxonomy.rank_names
    else
      self#real_action

  method private real_action =
    let rp = self#get_rp in
    let tax = Refpkg.get_taxonomy rp in
    begin
      let out_ch = open_out ((fv out_prefix) ^ "queryseq.txt") in
      let taxonomy = Tax_id.TaxIdMap.fold
        (fun tax_id (rank, parent) taxl ->
          (tax_id, rank, parent) :: taxl)
        new_taxonomy
        []
      in
      let taxonomy = List.sort (fun (_, a, _) (_, b, _) -> a - b) taxonomy in
      List.iter
        (fun (tax_id, rank, parent) ->
          Printf.fprintf out_ch
            "%s*%s*%s*%d*%s\n"
            (* oh god this is terrible *)
            (string_of_tax_id tax_id)
            (Tax_taxonomy.get_tax_name tax tax_id)
            (string_of_tax_id parent)
            (index_of included rank)
            (Tax_taxonomy.get_rank_name tax rank))
        taxonomy;
      close_out out_ch
    end
end
