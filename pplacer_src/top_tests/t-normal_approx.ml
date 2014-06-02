open New_normal_approx

let rng = Gsl.Rng.make Gsl.Rng.KNUTHRAN2002;;
Gsl.Rng.set rng (Nativeint.of_int 1);;

let pr1 = Placerun_io.of_file "top_tests/test1.place";;
let pr3 = Placerun_io.of_file "top_tests/test3.place";;

let m = normal_pair_approx rng Mass_map.Point Placement.ml_ratio pr1 pr3;;
