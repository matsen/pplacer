open MapsSets

let it = Stree_io.of_newick_str "((x:0.2,y:3e-2):0.05,z:1e-5):0."
let rates = [1.; 2.]
let mini = Alignment.read_align "tiny.fasta"
let model = Model.build "GTR" false "JC.stats.txt" mini rates
let (x,y) = Glv_of_arrthing.dp_glv_maps_of_ref model Alignment.Nucleotide_seq mini it [0;1;2;3;4]
let dest = Glv.zero 2 2 4
let src = IntMap.find 1 x
let q  = Glv.evolve_into 
         (fun bl -> model.Model.diagdq#expWithT bl)
         rates
         dest
         src 
         0.5


