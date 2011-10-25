(* just routines for arrays of Glv's.
*)

module Make (Model: Glvm.Model):
sig
  type t = Model.glv_t array
  val make: Model.t -> n_glvs:int -> n_sites:int -> t
  val copy: t -> t
  val mimic: t -> t
  val arr_get: t -> int -> Model.glv_t
  val iter: (Model.glv_t -> unit) -> t -> unit
  val get: t -> int -> Model.glv_t
  val get_one: t -> Model.glv_t
  val evolve_into: Model.t -> src:t -> dst:t -> (int -> float) -> unit
  val prep_supernodes: Model.t -> dst:t -> t -> t -> (int -> float) -> unit

end = struct
  module Glv = Model.Glv

  type t = Glv.t array

  let arr_get = Array.get

  let make model ~n_glvs ~n_sites =
    Array.init n_glvs (fun _ -> Model.make_glv model ~n_sites)

  let iter = Array.iter

  let copy a = Array.map Glv.copy a

  let mimic a = Array.map Glv.mimic a

  let get a glvi = arr_get a glvi

  let get_one a = assert(a <> [||]); a.(0)

  let evolve_into model ~src ~dst bl_fun =
    let n = Array.length src in
    if n <> Array.length dst then
      failwith "Glv_arr.evolve_into: unequal lengths!";
    for i=0 to n-1 do
      Model.evolve_into model ~src:src.(i) ~dst:dst.(i) (bl_fun i)
    done

(* for making a collection of nodes for the first (fast) evaluation *)
  let prep_supernodes model ~dst darr parr bl_fun =
    let n = Array.length dst in
    if n <> Array.length darr || n <> Array.length parr then
      failwith "Glv_arr.prep_supernode: unequal lengths!";
    let utild = Glv.mimic (get_one darr)
    and utilp = Glv.mimic (get_one parr) in
    for i=0 to n-1 do
      Model.evolve_into model ~src:darr.(i) ~dst:utild (bl_fun i);
      Model.evolve_into model ~src:parr.(i) ~dst:utilp (bl_fun i);
      Model.statd_pairwise_prod model ~dst:dst.(i) utild utilp
    done

end

