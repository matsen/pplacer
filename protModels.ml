(* pplacer v0.2. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *)

open MapsSets

type model = LG | WAG

(* *** translating protein sequences *** *)

let prot_code = [|'A'; 'R'; 'N'; 'D'; 'C'; 'Q'; 'E'; 'G'; 'H'; 'I'; 'L'; 'K'; 'M'; 'F'; 'P'; 'S'; 'T'; 'W'; 'Y'; 'V'|]

let prot_map = begin
  let pm = ref CharMap.empty in
  Array.iteri (
    fun i c -> 
      let v = Gsl_vector.create ~init:0. 20 in
      v.{i} <- 1.;
      pm := CharMap.add c v !pm;
  ) prot_code;
  let any = Gsl_vector.create ~init:1. 20 in
  CharMap.add '-' any (
    CharMap.add 'B' (CharMap.find 'N' !pm) ( (* asparagine is N or B *)
    CharMap.add 'Z' (CharMap.find 'Q' !pm) ( (* asparagine is Q or Z *)
    CharMap.add '?' any (
    CharMap.add 'X' any !pm))))
end

let likeArrOfAA aa = 
  try
    CharMap.find aa prot_map 
  with
    | Not_found -> 
        invalid_arg (Printf.sprintf "%c not a known amino acid!" aa)


(* *** MODELS *** *)

let parseLine line = 
  Array.of_list (List.map float_of_string (Str.split (Str.regexp "[ \t]+") line))
  (* BigarrayFuns.vecOfList (List.map float_of_string (Pcre.split ~pat:"\\s+"
   * line))*)

let parseProtModel stringArr = 
  let lowerTriMat = 
    Array.map parseLine stringArr
  in
  let nEntries = 1 + Array.length lowerTriMat in
  let qMat = 
    Fam_gsl_matvec.matInit nEntries nEntries (
      fun i j ->
        if i = j then 0. (* filled in later *)
        else lowerTriMat.((max i j)-1).(min i j)) in
  qMat

let parseFreq str = Gsl_vector.of_array (parseLine str)

let littleWag = 
parseProtModel [|
"0.551571";
"0.509848   0.635346";
"0.738998   0.147304  5.429420";
"1.027040   0.528191  0.265256   0.0302949";
|]

let wag_trans = 
parseProtModel [|
"0.551571";
"0.509848   0.635346";
"0.738998   0.147304  5.429420";
"1.027040   0.528191  0.265256   0.0302949";
"0.908598   3.035500  1.543640   0.616783   0.0988179";
"1.582850   0.439157  0.947198   6.174160   0.021352   5.469470";
"1.416720   0.584665  1.125560   0.865584   0.306674   0.330052   0.567717";
"0.316954   2.137150  3.956290   0.930676   0.248972   4.294110   0.570025   0.249410";
"0.193335   0.186979  0.554236   0.039437   0.170135   0.113917   0.127395   0.0304501  0.138190";
"0.397915   0.497671  0.131528   0.0848047  0.384287   0.869489   0.154263   0.0613037  0.499462   3.170970";
"0.906265   5.351420  3.012010   0.479855   0.0740339  3.894900   2.584430   0.373558   0.890432   0.323832   0.257555";
"0.893496   0.683162  0.198221   0.103754   0.390482   1.545260   0.315124   0.174100   0.404141   4.257460   4.854020  0.934276";
"0.210494   0.102711  0.0961621  0.0467304  0.398020   0.0999208  0.0811339  0.049931   0.679371   1.059470   2.115170  0.088836   1.190630";
"1.438550   0.679489  0.195081   0.423984   0.109404   0.933372   0.682355   0.243570   0.696198   0.0999288  0.415844  0.556896   0.171329   0.161444";
"3.370790   1.224190  3.974230   1.071760   1.407660   1.028870   0.704939   1.341820   0.740169   0.319440   0.344739  0.967130   0.493905   0.545931   1.613280";
"2.121110   0.554413  2.030060   0.374866   0.512984   0.857928   0.822765   0.225833   0.473307   1.458160   0.326622  1.386980   1.516120   0.171903   0.795384   4.378020";
"0.113133   1.163920  0.0719167  0.129767   0.717070   0.215737   0.156557   0.336983   0.262569   0.212483   0.665309  0.137505   0.515706   1.529640   0.139405   0.523742   0.110864";
"0.240735   0.381533  1.086000   0.325711   0.543833   0.227710   0.196303   0.103604   3.873440   0.420170   0.398618  0.133264   0.428437   6.454280   0.216046   0.786993   0.291148   2.485390";
"2.006010   0.251849  0.196246   0.152335   1.002140   0.301281   0.588731   0.187247   0.118358   7.821300   1.800340  0.305434   2.058450   0.649892   0.314887   0.232739   1.388230   0.365369   0.314730";
|]

let wag_statd = parseFreq "0.0866279  0.043972  0.0390894  0.0570451  0.0193078  0.0367281  0.0580589  0.0832518  0.0244313  0.048466   0.086209  0.0620286  0.0195027  0.0384319  0.0457631  0.0695179  0.0610127  0.0143859  0.0352742  0.0708956"


(* Le and Gascuel's matrix *)
let lg_trans = 
parseProtModel [|
"0.425093";
"0.276818 0.751878";
"0.395144 0.123954 5.076149";
"2.489084 0.534551 0.528768 0.062556";
"0.969894 2.807908 1.695752 0.523386 0.084808";
"1.038545 0.363970 0.541712 5.243870 0.003499 4.128591";
"2.066040 0.390192 1.437645 0.844926 0.569265 0.267959 0.348847";
"0.358858 2.426601 4.509238 0.927114 0.640543 4.813505 0.423881 0.311484";
"0.149830 0.126991 0.191503 0.010690 0.320627 0.072854 0.044265 0.008705 0.108882";
"0.395337 0.301848 0.068427 0.015076 0.594007 0.582457 0.069673 0.044261 0.366317 4.145067";
"0.536518 6.326067 2.145078 0.282959 0.013266 3.234294 1.807177 0.296636 0.697264 0.159069 0.137500";
"1.124035 0.484133 0.371004 0.025548 0.893680 1.672569 0.173735 0.139538 0.442472 4.273607 6.312358 0.656604";
"0.253701 0.052722 0.089525 0.017416 1.105251 0.035855 0.018811 0.089586 0.682139 1.112727 2.592692 0.023918 1.798853";
"1.177651 0.332533 0.161787 0.394456 0.075382 0.624294 0.419409 0.196961 0.508851 0.078281 0.249060 0.390322 0.099849 0.094464";
"4.727182 0.858151 4.008358 1.240275 2.784478 1.223828 0.611973 1.739990 0.990012 0.064105 0.182287 0.748683 0.346960 0.361819 1.338132";
"2.139501 0.578987 2.000679 0.425860 1.143480 1.080136 0.604545 0.129836 0.584262 1.033739 0.302936 1.136863 2.020366 0.165001 0.571468 6.472279";
"0.180717 0.593607 0.045376 0.029890 0.670128 0.236199 0.077852 0.268491 0.597054 0.111660 0.619632 0.049906 0.696175 2.457121 0.095131 0.248862 0.140825";
"0.218959 0.314440 0.612025 0.135107 1.165532 0.257336 0.120037 0.054679 5.306834 0.232523 0.299648 0.131932 0.481306 7.803902 0.089613 0.400547 0.245841 3.151815";
"2.547870 0.170887 0.083688 0.037967 1.959291 0.210332 0.245034 0.076701 0.119013 10.649107 1.702745 0.185202 1.898718 0.654683 0.296501 0.098369 2.188158 0.189510 0.249313";
|]

let lg_statd = parseFreq "0.079066 0.055941 0.041977 0.053052 0.012937 0.040767 0.071586 0.057337 0.022355 0.062157 0.099081 0.064600 0.022951 0.042302 0.044040 0.061197 0.053287 0.012066 0.034155 0.069147"

let trans_and_statd_of_model_name = function
  | "LG" -> (lg_trans, lg_statd)
  | "WAG" -> (wag_trans, wag_statd)
  | model_name -> failwith ("unknown model: "^model_name)

(* let qs = Diagd.ofSymmAAR wag_trans

let x = Mat.AAR.mul_vec wagDiagd#toMatrix wagFreq

let sums = Array.map (Array.fold_left ( +. ) 0.) (wagDiagd#toMatrix)

let m = wagDiagd#toMatrix

let f = Mat.AAR.mul_rowVec wagFreq m
 *)


    (*
     
to test, just paste this into the toplevel:

#directory "/home/matsen/pplacer/ocaml/";;

#use "protModels.ml";;     
#cd "/home/matsen/pplacer/ocaml/compareProteinLikes";;
let test = Alignment.readAlign "SEL0F.place.phy";;
let testAL = AlignmentLike.ofUnnamedProtAlignment (Alignment.forgetNames test);;
let (treeNames, untransTree) = Liketree.ofNewickFile "SEL0F.place.phy_phyml_tree.txt";;
let alignNames = Array.map fst test;; 
let translatedTree = LiketreeFuns.translateTaxonNumbers (PplacerFuns.makeTranslationArr treeNames alignNames) untransTree;;

let ll = lgLL translatedTree testAL;;

phyml Log-likelihood: 			-4929.83438

let sl = Common_base.stringListOfFile "frag.place.phy_phyml_stats.txt";;
let diagd = Diagd.normalizedOfExchangeableMat b d;;
let (b,d) = parseNucModel sl;;
     *)


