open Ppatteries
open Subcommand
open Guppy_cmdobjs

let save_named_fal fname fal =
  Csv.save fname (Guppy_splitify.fal_to_strll fal)

class virtual pca_cmd () =
object (self)
  inherit subcommand () as super
  inherit output_cmd ~show_fname:false ~prefix_required:true () as super_output
  inherit mass_cmd () as super_mass
  inherit heat_cmd () as super_heat
  inherit refpkg_cmd ~required:false as super_refpkg
  inherit placefile_cmd () as super_placefile

  val write_n = flag "--write-n"
    (Formatted (5, "The number of principal coordinates to write out (default is %d)."))
  val som = flag "--som"
    (Formatted (0, "The number of dimensions to rotate for support overlap minimization\
    (default is %d; options are 0, 2, 3)."))
  val scale = flag "--scale"
    (Plain (false, "Scale variances to one before performing principal components."))
  val symmv = flag "--symmv"
    (Plain (false, "Use a complete eigendecomposition rather than power iteration."))
  val raw_eval = flag "--raw-eval"
    (Plain (false, "Output the raw eigenvalue rather than the fraction of variance."))

  method specl =
    super_output#specl
    @ super_mass#specl
    @ super_refpkg#specl
    @ super_heat#specl
    @ [
      int_flag write_n;
      int_flag som;
      toggle_flag scale;
      toggle_flag symmv;
      toggle_flag raw_eval;
    ]

  method private virtual prep_data : 'prl_t -> 'data_t
  method private virtual check_data : 'data_t -> int -> int
  method private virtual gen_pca : use_raw_eval:bool -> scale:bool -> symmv:bool -> int -> 'data_t -> 'prl_t -> 'result_t
  method private virtual post_pca : 'result_t -> 'data_t -> 'prl_t -> unit

  method private check_uniqueness fal write_n =
    let n_unique_rows = List.length (List.sort_unique compare fal) in
    if n_unique_rows <= 2 then
      failwith (Printf.sprintf "You have only %d unique row(s) in your data \
      after transformation. This is not enough to do PCA." n_unique_rows);
    if n_unique_rows < write_n then
      Printf.printf "You have only %d unique rows in your data after \
          transformation. Restricting to this number of principal components.\n"
        n_unique_rows;
    min n_unique_rows write_n

  method private placefile_action prl =
    self#check_placerunl prl;
    let data = self#prep_data prl in
    let write_n = self#check_data data (fv write_n) in
    let result =
      self#gen_pca ~use_raw_eval:(fv raw_eval)
        ~scale:(fv scale) ~symmv:(fv symmv) write_n data prl
    in
    self#post_pca result data prl

end
