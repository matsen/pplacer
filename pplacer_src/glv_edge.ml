(* Makes a type for edges, with cached glv calculations.
 * Evolv is orig evolved along an edge of length bl.
 *
 * Scary side-effect land! But by keeping with the interface, we should be safe.
 *)

module Make (Model: Glvm.Model):
sig
  type t
  val make: Model.t -> Model.glv_t -> float -> t
  val get_bl: t -> float
  val get_orig: t -> Model.glv_t
  val get_evolv: t -> Model.glv_t
  val set_bl: Model.t -> t -> float -> unit

end = struct
  module Glv = Model.Glv
  type t = {
    orig: Glv.t;
    evolv: Glv.t;
    bl: float ref;
  }

  let get_bl e = !(e.bl)
  let get_orig e = e.orig
  let get_evolv e = e.evolv

  let recalculate model glve =
    Model.evolve_into model
      ~dst:(glve.evolv) ~src:(glve.orig) !(glve.bl)

  let set_bl model glve new_bl =
    glve.bl := new_bl;
    recalculate model glve

  let make model orig init_bl =
    let glve =
      { orig;
        evolv = Glv.copy orig;
        bl = ref init_bl } in
    recalculate model glve;
    glve

end
