open Ppatteries

module BA = Bigarray
module BA1 = BA.Array1
module BA2 = BA.Array2
module BA3 = BA.Array3

type init_params =
  | Invalid
  | Gmix_model of string * bool * float array option * float array

module type Model =
sig
  type t
  type glv_t
  val statd: t -> Gsl_vector.vector
  val diagdq: t -> Diagd.t
  val rates: t -> float array
  val tensor: t -> Tensor.tensor
  val seq_type: t -> Alignment.seq_type
  val n_states: t -> int
  val n_rates: t -> int
  val build: Alignment.t -> init_params -> t

  val prep_tensor_for_bl: t -> float -> unit
  val get_symbol: char array -> int -> char
  val to_sym_str: char array -> int array -> string
  val code: t -> char array

  module Glv:
  sig
    type t = glv_t
    val get_n_rates: t -> int
    val get_n_sites: t -> int
    val get_n_states: t -> int
    val ppr: Format.formatter -> t -> unit
    val mimic: t -> t
    val copy: t -> t
    val set_exp_and_all_entries: t -> int -> float -> unit
    val set_all: t -> int -> float -> unit
    val fp_classify: t -> fpclass
    val mask_into: bool array -> src:t -> dst:t -> unit
    val perhaps_pull_exponent: int -> t -> unit
    val bounded_logdot: Gsl_vector.vector -> t -> t -> int -> int -> float
    val logdot: Gsl_vector.vector -> t -> t -> float
    val listwise_prod: t -> t list -> unit
    val get_a: t -> rate:int -> site:int -> state:int -> float
    val prep_constant_rate_glv_from_lv_arr: t -> Gsl_vector.vector array -> unit
  end

  val make_glv: t -> n_sites:int -> Glv.t
  val lv_arr_to_constant_rate_glv: t -> int -> Gsl_vector.vector array -> Glv.t
  val log_like3: t -> Gsl_vector.vector -> Glv.t -> Glv.t -> Glv.t -> float
  val slow_log_like3: t -> Glv.t -> Glv.t -> Glv.t -> float
  val evolve_into: t -> dst:Glv.t -> src:Glv.t -> float -> unit
  val statd_pairwise_prod: t -> dst:Glv.t -> Glv.t -> Glv.t -> unit

end

type t = (module Model) * init_params

