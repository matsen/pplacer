open Subcommand
open Guppy_cmdobjs

let tab = Str.regexp "\t"

class cmd () =
object (self)
  inherit subcommand () as super
  inherit refpkg_cmd ~required:true as super_refpkg

  val csv_out = flag "--csv"
    (Plain (false, "Write .class.csv files containing CSV data."))

  method specl = toggle_flag csv_out :: super_refpkg#specl

  method desc = "converts RDP output to something resmbling guppy classify output"
  method usage = "usage: classify_rdp -c some.refpkg rdp_output_file[s]"

  method action argl =
    let rp = self#get_rp in
    let td = Refpkg.get_taxonomy rp in
    let name_map = Tax_id.TaxIdMap.fold
      (fun tax_id name ->
        MapsSets.StringMap.add
          name
          (Tax_id.to_string tax_id))
      td.Tax_taxonomy.tax_name_map
      MapsSets.StringMap.empty
    in

    let out_func name outal =
      if fv csv_out then
        let ch = open_out (name ^ ".class.csv") in
        output_string ch "name,desired_rank,rank,tax_id,likelihood,origin\n";
        List.iter
          (fun arr -> Printf.fprintf ch
            "%s,%s\n"
            (String.concat "," (Array.to_list arr))
            name)
          outal;
        close_out ch
      else
        let ch = open_out (name ^ ".class.tab") in
        String_matrix.write_padded ch (Array.of_list outal);
        close_out ch
    in

    List.iter
      (fun rdpfile ->
        let lines = File_parsing.string_list_of_file rdpfile in
        let lines' = Base.map_and_flatten
          (fun line ->
            (* past participle of 'to split' *)
            let splut = Array.of_list (Str.split tab line) in
            List.fold_left
              (fun accum idx ->
                [|
                  splut.(0);
                  splut.(idx + 1);
                  splut.(idx + 1);
                  (MapsSets.StringMap.find splut.(idx) name_map);
                  splut.(idx + 2);
                |] :: accum)
              []
              [8; 11; 14; 17; 20]
          )
          lines
        in
        out_func rdpfile lines')
      argl
end
