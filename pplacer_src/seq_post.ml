(* Functions for summarizing the site-wise posterior probability of base pairs
 * at given sites for internal positions on the tree. *)
open Ppatteries

module Make (Model: Glvm.Model) =
struct
  module Glv = Model.Glv
  module Glv_arr = Glv_arr.Make(Model)

  (* Get the most likely vector at a chosen node id.
   * atarr is the Glv_arr which gives the Glv on the side we are interested in,
   * while neigharr is the neighbor.
   * u1 and u2 are utility vectors like Glv.mimic (Glv_arr.get_one darr) *)
  let get_posterior ~dst util model t ~atarr ~neigharr id =
    Model.evolve_into model
      ~dst:util
      ~src:(Glv_arr.arr_get neigharr id)
      (Gtree.get_bl t id);
    Model.statd_pairwise_prod
      model ~dst util (Glv_arr.arr_get atarr id)

  type pos = Distal | Proximal

  let get_summary pos summarize_f initial u1 u2 model t ~darr ~parr id =
    let (atarr, neigharr) = match pos with
      | Distal -> (darr, parr)
      | Proximal -> (parr, darr)
    in
    get_posterior ~dst:u1 u2 model t ~atarr ~neigharr id;
    Glv.summarize_post summarize_f initial u1

end
