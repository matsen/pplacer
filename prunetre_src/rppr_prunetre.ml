open Subcommand
open Guppy_cmdobjs
open Fam_batteries
open MapsSets

class cmd () =
object (self)
  inherit subcommand () as super
  inherit output_cmd () as super_output

  val cutoff = flag "--cutoff"
    (Needs_argument ("cutoff", "Specify the maximum branch length to be trimmed."))
  val leaf_count = flag "--leaves"
    (Needs_argument ("leaves", "Specify the maximum number of leaves to leave un-trimmed."))
  val names_only = flag "--names-only"
    (Plain (false, "Only split out a list of names, rather than names and PD decrease."))
  val safe = flag "--unsafe"
    (Plain (true, "Don't perform internal checks."))
  val never_prune_from = flag "--never-prune-from"
    (Plain ("", "Provide a file containing taxa that will not be pruned."))
  val never_prune_regex_from = flag "--never-prune-regex-from"
    (Plain ("", "Provide a file containing regular expressions; taxa matching one of these will not be pruned."))

  method specl = super_output#specl @ [
    float_flag cutoff;
    int_flag leaf_count;
    toggle_flag names_only;
    toggle_flag safe;
    string_flag never_prune_from;
    string_flag never_prune_regex_from;
  ]

  method desc = "prunes the tree"
  method usage = "usage: prunetre [options] tree"

  method action = function
    | [fname] ->
      let never_prune_names =
        match fv never_prune_from with
          | "" -> StringSet.empty
          | fname ->
            StringSet.of_list (File_parsing.string_list_of_file fname)
      and never_prune_regexl =
        match fv never_prune_regex_from with
          | "" -> []
          | fname -> List.map Str.regexp (File_parsing.string_list_of_file fname)
      and names_only = fv names_only
      and safe = fv safe
      and criterion = match fvo cutoff, fvo leaf_count with
        | Some cutoff, None ->
          if cutoff <= 0. then
            failwith "Please specify a positive cutoff value.";
          Pd.Branch_length cutoff
        | None, Some count -> Pd.Leaf_count count
        | _, _ -> failwith "please specify exactly one of --cutoff and --leaves"
      in
      let ss_list_add = List.fold_right StringSet.add
      and search r str =
        try let _ = Str.search_forward r str 0 in true with
          | Not_found -> false
      in
      let gt = Newick_gtree.of_file fname in
      let get_name id = (IntMap.find id gt.Gtree.bark_map)#get_name in
      let namel =
        Gtree.recur
          (fun _ below -> List.flatten below)
          (fun i -> [get_name i])
          gt
      in
      let never_prunes =
        List.fold_right
          (fun r -> ss_list_add (List.filter (search r) namel))
          never_prune_regexl
          never_prune_names
      in
      StringSet.iter (Printf.printf "not pruning %s\n") never_prunes;
      let pt = Ptree.of_gtree gt
      and never_prune_ids =
        IntMap.fold
          (fun i b s ->
            match b#get_name_opt with
              | Some name ->
                if StringSet.mem name never_prunes then IntSet.add i s
                else s
              | None -> s)
          gt.Gtree.bark_map
          IntSet.empty
      in
      let line_of_result =
        if names_only then (fun (id,_,_) -> [get_name id])
        else (fun (id,bl,_) -> [get_name id; string_of_float bl])
      in
      Csv.save_out self#out_channel
        (List.map
           line_of_result
           (Pd.until_stopping safe never_prune_ids criterion pt))

    | _ -> failwith "prunetre takes exactly one tree"

end

