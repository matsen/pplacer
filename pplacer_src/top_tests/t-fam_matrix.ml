open Fam_matrix;;

let m = init 4 4 (fun i j -> (float_of_int (i*j)));;

let t = trace m;;
