open MapsSets

let version = "v0.4"

(* *** regexps and utils *** *)
let split_on_space s = Str.split (Str.regexp "[ \t]+") s
let place_file_rex = Str.regexp ".*\\.place"

(* *** accessing placefiles *** *)
(* our strategy is to load the placefiles in to memory when we need them, but if
  * they are already in memory, then we use them *)
let placerun_map = ref StringMap.empty
let placerun_by_name fname =
  if not (Str.string_match place_file_rex fname 0) then
    failwith ("Place files must end with .place suffix, unlike: "^fname);
  if StringMap.mem fname !placerun_map then
    StringMap.find fname !placerun_map
  else begin
    let pr = Placerun_io.filtered_of_file fname in
    if 0 = Placerun.n_pqueries pr then failwith (fname^" has no placements!");
    placerun_map := StringMap.add fname pr !placerun_map;
    pr
  end

(* *** wrapped versions of programs *** *)

let pr_wrap_parse_argv argl specl usage =
  List.map placerun_by_name (Subcommand.wrap_parse_argv argl specl usage)

(* here are the commands, wrapped up to simply take an argument list. they must
 * also print out a documentation line when given an empty list argument.
 *
 * can't wait for first class modules in 3.12!!!
 * that will make all of this quite slick...
 *
 * Note that we can't push them out to their respective subcommand files because
 * they are using placerun_by_name.
 *)
let bary_of_argl = function
  | [] -> print_endline "draws the barycenter of a placement collection on the reference tree"
  | argl ->
    let prefs = Mokaphy_bary.Prefs.defaults () in
    Mokaphy_bary.bary
      prefs
      (pr_wrap_parse_argv
        argl
        (Mokaphy_bary.Prefs.specl_of_prefs prefs)
        "usage: bary [options] placefile[s]")

let heat_of_argl = function
  | [] -> print_endline "makes a heat tree given two placefiles"
  | argl ->
    let prefs = Mokaphy_heat.Prefs.defaults () in
    Mokaphy_heat.heat
      prefs
      (pr_wrap_parse_argv
        argl
        (Mokaphy_heat.Prefs.specl_of_prefs prefs)
        "usage: heat [options] ex1.place ex2.place")

let kr_of_argl = function
  | [] -> print_endline "runs KR analyses, including significance estimation"
  | argl ->
    let prefs = Mokaphy_kr.Prefs.defaults () in
    Mokaphy_kr.kr
      prefs
      (pr_wrap_parse_argv
        argl
        (Mokaphy_kr.Prefs.specl_of_prefs prefs)
        "usage: kr [options] placefiles")

let pd_of_argl = function
  | [] -> print_endline "calculates PD of the subtree spanned by the placements"
  | argl ->
    let prefs = Mokaphy_pd.Prefs.defaults () in
    Mokaphy_pd.pd
      prefs
      (pr_wrap_parse_argv
        argl
        (Mokaphy_pd.Prefs.specl_of_prefs prefs)
        "usage: pd [options] placefiles")

let unifrac_of_argl = function
  | [] -> print_endline "calculates the pairwise PD fraction of the subtree spanned by the placments"
  | argl ->
    let prefs = Mokaphy_unifrac.Prefs.defaults () in
    Mokaphy_unifrac.unifrac
      prefs
      (pr_wrap_parse_argv
        argl
        (Mokaphy_unifrac.Prefs.specl_of_prefs prefs)
        "usage: unifrac [options] placefiles")

let uavgdist_of_argl = function
  | [] -> print_endline "calculates the unary pairwise distance for each place file"
  | argl ->
    let prefs = Mokaphy_avgdist.Prefs.defaults () in
    Mokaphy_avgdist.uavgdist
      prefs
      (pr_wrap_parse_argv
        argl
        (Mokaphy_avgdist.Prefs.specl_of_prefs prefs)
        "usage: uavgdist [options] placefiles")

let bavgdist_of_argl = function
  | [] -> print_endline "calculates the binary pairwise distance for each place file"
  | argl ->
    let prefs = Mokaphy_avgdist.Prefs.defaults () in
    Mokaphy_avgdist.bavgdist
      prefs
      (pr_wrap_parse_argv
        argl
        (Mokaphy_avgdist.Prefs.specl_of_prefs prefs)
        "usage: bavgdist [options] placefiles")

let cluster_of_argl = function
  | [] -> print_endline "makes a heirarchical cluster of the placeruns"
  | argl ->
    let prefs = Mokaphy_cluster.Prefs.defaults () in
    Mokaphy_cluster.cluster
      prefs
      (pr_wrap_parse_argv
        argl
        (Mokaphy_cluster.Prefs.specl_of_prefs prefs)
        "usage: cluster [options] placefiles")

let clusterviz_of_argl = function
  | [] -> print_endline "makes a nice tree for visualization of results"
  | argl ->
    let prefs = Mokaphy_clusterviz.Prefs.defaults () in
    Mokaphy_clusterviz.clusterviz
      prefs
      (Subcommand.wrap_parse_argv
        argl
        (Mokaphy_clusterviz.Prefs.specl_of_prefs prefs)
        "usage: clusterviz [options] --name-csv my.csv cluster_dir")

let bootviz_of_argl = function
  | [] -> print_endline "makes a tree which shows the bootstrap values"
  | argl ->
    let prefs = Mokaphy_bootviz.Prefs.defaults () in
    Mokaphy_bootviz.bootviz
      prefs
      (Subcommand.wrap_parse_argv
        argl
        (Mokaphy_bootviz.Prefs.specl_of_prefs prefs)
        "usage: bootviz [options] -b boot_trees cluster_tree")

let bootsub_of_argl = function
  | [] -> print_endline "Removes unclusterable samples"
  | argl ->
    let prefs = Mokaphy_bootsub.Prefs.defaults () in
    Mokaphy_bootsub.bootsub
      prefs
      (Subcommand.wrap_parse_argv
        argl
        (Mokaphy_bootsub.Prefs.specl_of_prefs prefs)
        "usage: bootsub [options] -b boot_trees --name-csv my.csv cluster_tree")

let pca_of_argl = function
  | [] -> print_endline "does PCA, and makes lovely trees"
  | argl ->
    let prefs = Mokaphy_pca.Prefs.defaults () in
    Mokaphy_pca.pca
      prefs
      (pr_wrap_parse_argv
        argl
        (Mokaphy_pca.Prefs.specl_of_prefs prefs)
        "usage: pca [options] placefiles")


let cmd_map =
  List.fold_right
    (fun (k,v) -> StringMap.add k v)
    [
      "bary", bary_of_argl;
      "heat", heat_of_argl;
      "kr", kr_of_argl;
      "pd", pd_of_argl;
      "unifrac", unifrac_of_argl;
      "uavgdist", uavgdist_of_argl;
      "bavgdist", bavgdist_of_argl;
      "cluster", cluster_of_argl;
      "clusterviz", clusterviz_of_argl;
      "bootviz", bootviz_of_argl;
      "bootsub", bootsub_of_argl;
      "pca", pca_of_argl;
    ]
    StringMap.empty

let process_batch_file fname =
  List.iter
    (fun s -> Subcommand.process_cmd "mokaphy" cmd_map (split_on_space s))
    (File_parsing.filter_comments
      (File_parsing.string_list_of_file fname))

let () = begin
    Arg.parse
      [
        "-B", Arg.String process_batch_file,
        "Execute commands from indicated batch file";
        "-v", Arg.Unit (fun () -> Printf.printf "mokaphy %s\n" version),
        "Print version and exit";
        "--cmds",
          Arg.Unit (fun () -> Subcommand.print_avail_cmds "mokaphy" cmd_map),
        "Print a list of the available commands.";
      ]
      (fun _ -> (* anonymous args. tl to remove "mokaphy" or symlink name *)
        Subcommand.process_cmd
          "mokaphy"
          cmd_map
          (List.tl (Array.to_list Sys.argv));
        exit 0) (* need to exit to avoid processing the other anon args as cmds *)
      "mokaphy can be used as mokaphy [command name] [args] \
      or -B [batch file] to run a batch analysis; \
      Type mokaphy --cmds to see the list of available commands."
end
