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
  inherit output_cmd ~prefix_required:true () as super_output
  inherit refpkg_cmd ~required:true as super_refpkg

  val included_ranks = flag "-r"
    (Plain ([], "Ranks to include in the annotated fasta file. Can be comma-separated."))
  val list_ranks = flag "-l"
    (Plain (false, "List all of the ranks, by number."))

  method specl =
    super_output#specl
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
    let included = Base.map_and_flatten
      (Str.split (Str.regexp ","))
      (fv included_ranks)
    in
    let included = List.sort (-) (List.map int_of_string included) in
    let prefix = self#single_prefix ~requires_user_prefix:true () in

    let new_taxonomy = begin
      let seqinfo = Refpkg.get_seqinfom rp
      and aln = Refpkg.get_aln_fasta rp
      and out_ch = open_out (prefix ^ "queryseq.fasta") in
      let ret = Array.fold_left
        (fun tax_map (name, seq) ->
          let tax_id = Tax_seqinfo.tax_id_by_name seqinfo name
          and seq = Str.global_replace gap_regexp "" seq
          and get_rank = Tax_taxonomy.get_tax_rank tax in
          let rec aux tax_pairs backlog tax_ids ranks =
            match tax_ids, ranks, backlog with
              | i :: il, j :: jl, _ when get_rank i = j ->
                aux ((i, j) :: tax_pairs) [] il jl
              | i :: il, (j :: _ as jl), _ when get_rank i < j ->
                aux tax_pairs (i :: backlog) il jl
              | (i :: _ as il), j :: jl, top :: backlog when get_rank i > j ->
                aux ((top, j) :: tax_pairs) backlog il jl
              | _, _, _ -> tax_pairs
          in
          let tax_pairs = aux
            []
            []
            (Tax_taxonomy.get_lineage tax tax_id)
            included
          in
          Printf.fprintf out_ch ">%s " name;
          List.iter
            (fun (tax_id, _) ->
              Printf.fprintf
                out_ch
                "%s;"
                (Tax_taxonomy.get_tax_name tax tax_id))
            (List.rev tax_pairs);
          Printf.fprintf out_ch "\n%s\n" seq;
          let rec aux accum = function
            | [] -> accum
            | (tax_id, rank) :: rest ->
              aux
                (Tax_id.TaxIdMap.add
                   tax_id
                   (rank, match rest with
                     | [] -> Tax_id.of_string "0"
                     | (parent, _) :: _ -> parent)
                   accum)
                rest
          in
          Tax_id.TaxIdMap.union
            tax_map
            (aux Tax_id.TaxIdMap.empty tax_pairs))
        Tax_id.TaxIdMap.empty
        aln
      in
      close_out out_ch;
      ret
    end
    in

    begin
      let out_ch = open_out (prefix ^ "queryseq.txt") in
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
