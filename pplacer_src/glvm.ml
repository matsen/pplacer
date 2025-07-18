open Ppatteries

type model_class = Gmix_model | Gcat_model

type init_params =
  (* Gmix_model (model_name, empirical_freqs, opt_transitions, rates) *)
  | Gmix_model_i of string * bool * float array option * float array
  (* Gcat_model (model_name, empirical_freqs, transitions, rates, site_categories) *)
  | Gcat_model_i of string * bool * float array option * float array * int array

module type Model =
sig
  type t
  type glv_t
  val build: Alignment.t -> init_params -> t
  val seq_type: t -> Alignment.seq_type
  val rates: t -> float array
  val refine: t -> int -> Newick_gtree.t ->
    Gsl.Vector.vector array IntMap.t -> glv_t array -> glv_t array -> unit
  val check: t -> Alignment.t -> unit
  val mask_sites: t -> bool array -> unit
  val write: unit IO.output -> t -> unit
  val get_model_class: unit -> model_class

  module Glv:
  sig
    type t = glv_t
    val get_n_sites: t -> int
    val ppr: Format.formatter -> t -> unit
    val mimic: t -> t
    val copy: t -> t
    val set_unit: t -> unit
    val fp_classify: t -> fpclass
    val mask_into: bool array -> src:t -> dst:t -> unit
    val perhaps_pull_exponent: int -> t -> unit
    val masked_logdot: Gsl.Vector.vector -> t -> t -> Linear.uint16_vector -> float
    val bounded_logdot: Gsl.Vector.vector -> t -> t -> int -> int -> float
    val logdot: Gsl.Vector.vector -> t -> t -> float
    val listwise_prod: t -> t list -> unit
    val prep_constant_rate_glv_from_lv_arr: t -> Gsl.Vector.vector array -> unit
    val summarize_post: (Gsl.Vector.vector -> 'a) -> 'a -> t -> 'a array
  end

  val make_glv: t -> n_sites:int -> Glv.t
  val mmap_glv_arrays:
    t -> Unix.file_descr -> bool -> int -> int -> n_sites:int -> Glv.t array array
  val size_of_glv_arrays: t -> int -> int -> n_sites:int -> int
  val make_constant_rate_glv_from_lv_arr: t -> Gsl.Vector.vector array -> Glv.t
  val log_like3: t -> Gsl.Vector.vector -> Glv.t -> Glv.t -> Glv.t -> float
  val site_log_like_arr3: t -> Glv.t -> Glv.t -> Glv.t -> float array
  val slow_log_like3: t -> Glv.t -> Glv.t -> Glv.t -> float
  val evolve_into: t -> ?reind_arr:int array -> dst:Glv.t -> src:Glv.t -> float -> unit
  val statd_pairwise_prod: t -> dst:Glv.t -> Glv.t -> Glv.t -> unit

end

type t = (module Model) * init_params

let get_symbol code = function
  | -1 -> '-'
  | i -> try code.(i) with | Invalid_argument _ -> assert(false)

let to_sym_str code ind_arr =
  StringFuns.of_char_array (Array.map (get_symbol code) ind_arr)

let code = function
  | Alignment.Nucleotide_seq -> Nuc_models.nuc_code
  | Alignment.Protein_seq -> Prot_models.prot_code
