open OUnit
open Test_util


type like_test_info = {
  dir_name: string;
  fasta_fname: string;
  tree_fname: string;
  correct_like: float;
}

let like_test info () =
  let d str = (info.dir_name^str) in
  let aln = Alignment_funs.upper_aln_of_any_file (d info.fasta_fname)
  and tree = Newick_gtree.of_file (d info.tree_fname)
  in
  let model = Model.of_json (d "phylo_model.json") aln
  and n_sites = Alignment.length aln
  in
  let check our_like =
    (Printf.sprintf "likelihood error: %g" our_like) @?
      (cmp_float ~epsilon:0.1 info.correct_like our_like)
  in
  (* Copied from pplacer_run.ml *)
  let like_aln_map =
    Like_stree.like_aln_map_of_data (Model.seq_type model) aln tree
  in
  let darr = Like_stree.glv_arr_for model tree n_sites in
  let parr = Glv_arr.mimic darr
  and snodes = Glv_arr.mimic darr
  in
  let util_glv = Glv.mimic (Glv_arr.get_one snodes) in
  Like_stree.calc_distal_and_proximal model tree like_aln_map
    util_glv ~distal_glv_arr:darr ~proximal_glv_arr:parr
    ~util_glv_arr:snodes;
  List.iter
    (Glv_arr.iter (Glv.perhaps_pull_exponent (-10)))
    [darr; parr;];
  let half_bl_fun loc = (Gtree.get_bl tree loc) /. 2. in
  Glv_arr.prep_supernodes model ~dst:snodes darr parr half_bl_fun;
  let utilv_nsites = Gsl_vector.create n_sites
  and util_d = Glv.mimic darr.(0)
  and util_p = Glv.mimic parr.(0)
  and util_one = Glv.mimic darr.(0)
  in
  Glv.set_all util_one 0 1.;
  for i=0 to (Array.length darr)-1 do
    let d = darr.(i)
    and p = parr.(i)
    and sn = snodes.(i)
    in
    Glv.evolve_into model ~src:d ~dst:util_d (half_bl_fun i);
    Glv.evolve_into model ~src:p ~dst:util_p (half_bl_fun i);
    check (Glv.slow_log_like3 model util_d util_p util_one);
    check (Glv.logdot utilv_nsites sn util_one);
  done


let jtt_info = {
  dir_name = "/home/matsen/pplacer/ocaml/tests/data/like/jtt/";
  fasta_fname = "actin.fasta";
  tree_fname = "actin.phy_phyml_tree.txt";
  correct_like = -2326.22075;
}


let suite = [
  "jtt" >:: (like_test jtt_info);
]

