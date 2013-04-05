open Ppatteries
open Guppy_cmdobjs

class cmd () =
object (self)
  inherit Guppy_pca.pca_cmd () as super_pca

  method specl =
    super_pca#specl

  method desc =
    "performs length principal components"
  method usage = "usage: lpca [options] placefiles"

  method private prep_data prl =
    let weighting, criterion = self#mass_opts
    and _, t = self#get_rpo_and_tree (List.hd prl)
    in
    let sl =
      List.map
        (fun pr ->
          Mass_map.Indiv.sort (Mass_map.Indiv.of_placerun weighting criterion pr))
        prl
    in
    (* TODO: remove the debugging print statement *)
    let result = Lpca.mat_print "F'LF" (Lpca.gen_lpca sl t) in
    let data = Array.to_list (Gsl_matrix.to_arrays result) in
    (data, ())

  method private gen_pca ~use_raw_eval ~scale ~symmv write_n data _ _ =
    let faa = Array.of_list data in
    Pca.gen_pca ~use_raw_eval ~scale ~symmv write_n faa

  method private post_pca (eval, evect) _ =
    let combol = (List.combine (Array.to_list eval) (Array.to_list evect)) in
    (combol, combol)
end
