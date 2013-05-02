open Ppatteries
open Subcommand
open Guppy_cmdobjs

let gap_regexp = Str.regexp "-"
let tax_name_regexp = Str.regexp "[ ;()]"

let get_mothur_name td tid =
  let name = Tax_taxonomy.get_tax_name td tid in
  Printf.sprintf
    "%s_%s"
    (Tax_id.to_string tid)
    (Str.global_replace tax_name_regexp "_" name)

class cmd () =
object (self)
  inherit subcommand () as super
  inherit output_cmd ~prefix_required:true () as super_output
  inherit refpkg_cmd ~required:true as super_refpkg

  val included_ranks = flag "-r"
    (Plain ([], "Ranks to include in the annotated fasta file. Can be comma-separated."))

  method specl =
    super_output#specl
  @ super_refpkg#specl
  @ [delimited_list_flag included_ranks]

  method desc = "convert a reference package to a format RDP wants"
  method usage = "usage: to_rdp -c my.refpkg -o prefix"

  method private action _ =
    let rp = self#get_rp in
    let tax = Refpkg.get_taxonomy rp in
    let included = fv included_ranks
      |> List.map (fun rk -> Array.findi ((=) rk) tax.Tax_taxonomy.rank_names)
      |> List.sort compare
    in
    if List.is_empty included then
      failwith "to_rdp must be passed at least one rank";
    let prefix = self#single_prefix ~requires_user_prefix:true () in
    let seqinfo = Refpkg.get_seqinfom rp
    and aln = Refpkg.get_aln_fasta rp
    and out_ch = open_out (prefix ^ ".tax") in
    Array.filter_map
      (fun (name, seq) ->
        let tax_id = Tax_seqinfo.tax_id_by_node_label seqinfo name
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
        if List.length tax_pairs <> List.length included then begin
          Printf.printf
            "warning: %s was excluded because it can't be classified \
             at all the specified ranks\n"
            name;
          None
        end else begin
          Printf.fprintf out_ch "%s\t" name;
          List.iter
            (fst
             |- get_mothur_name tax
             |- Printf.fprintf out_ch "%s;")
            (List.rev tax_pairs);
          Printf.fprintf out_ch "\n";
          Some (name, Str.global_replace gap_regexp "" seq)
        end)
      aln;
    |> flip Alignment.to_fasta (prefix ^ ".fasta")

end
