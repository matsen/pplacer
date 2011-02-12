
module Prefs = struct
  type mokaphy_prefs = 
    {
      boot_fname: string ref;
      out_fname: string ref;
      cutoff: float ref;
      name_csv: string ref;
    }
  
  let boot_fname p = !(p.boot_fname)
  let out_fname p = !(p.out_fname)
  let cutoff p = !(p.cutoff)
  let name_csv p = !(p.name_csv)
  
  let defaults () = 
    { 
      boot_fname = ref "";
      name_csv = ref "";
      out_fname = ref "";
      cutoff = ref 0.95;
    }
  
  (* arguments *)
  let specl_of_prefs prefs = [
    "-b", Arg.Set_string prefs.boot_fname,
    "The file containing the bootstrapped trees, one per line.";
    "--name-csv", Arg.Set_string prefs.name_csv,
    "A CSV file containing two columns: \"numbers\", which are node numbers in the clustered tree, and \"names\", which are names for those nodes.";
    "-o", Arg.Set_string prefs.out_fname,
    "Specify an out filename.";
    "--cutoff", Arg.Set_float prefs.cutoff,
    "Specify the cutoff for writing out the bootstrap value.";
    ]
end 



open MapsSets

module StrBootsub = 
  Bootsubfunc.Make 
    (MapsSets.OrderedString) 
    (MapsSets.StringSet) 
    (Cluster_common.StringSetSet)

let perform ch cutoff ~csv_fname ~boot_fname ~ct_fname = 
  if cutoff < 0. || cutoff > 1. then 
    failwith "bootsub cutoff must be between zero and one";
  let ct = Newick.of_file ct_fname
  and boot_tl = Newick.list_of_file boot_fname
  in
  let boot_sssl = List.map Cluster_common.sss_of_tree boot_tl
  and taxon_list t = List.map (Gtree.get_name t) (Gtree.leaf_ids t) 
  in
  let taxon_set t = List.fold_right StringSet.add (taxon_list t) StringSet.empty
  in
  let taxs = taxon_set ct
  and ssim = Cluster_common.ssim_of_tree ct
  in
  let ssim_find id ssim = 
    try IntMap.find id ssim with
    | Not_found -> 
        failwith (Printf.sprintf "Could not find %d in %s" id ct_fname)
  in
  List.iter 
    (fun ss -> if 0 <> StringSet.compare taxs ss then 
                 invalid_arg "taxon sets not identical")
    (List.map taxon_set boot_tl);
  List.iter
    (fun (id, name) ->
      Csv.save_out ch
        ([]::[name]::
          (List.map
            (fun (score, stro) ->
              [string_of_float score;
              match stro with None -> "" | Some s -> s])
            (StrBootsub.perform cutoff (ssim_find id ssim) boot_sssl))))
    (Cluster_common.numnamel_of_csv csv_fname)


let bootsub prefs = function
  | [ct_fname] -> begin
      match (Prefs.boot_fname prefs,
            Prefs.name_csv prefs) with
        | "",_ -> failwith "please supply a file of bootstrapped trees"
        | _,"" -> failwith "please supply a cluster CSV file"
        | (boot_fname, csv_fname) -> 
            Mokaphy_common.wrap_output (Prefs.out_fname prefs)
              (fun ch ->
                perform 
                  ch
                  (Prefs.cutoff prefs) 
                  ~csv_fname
                  ~boot_fname
                  ~ct_fname)
      end
  | [] -> () (* e.g. -help *)
  | _ -> failwith "Please specify exactly one cluster tree for bootsub."

