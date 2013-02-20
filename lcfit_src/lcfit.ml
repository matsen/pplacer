(* OCaml-C interface to lcfit_tripod and lcfit_pair *)

open Ppatteries

(* GSL exception, basically *)
exception Lcfit_err of (int * string)
let _ = Callback.register_exception "lcfit_err" (Lcfit_err (0, "any string"))

(* Debug code *)
let pq = ref 0
let pquery_name = ref ""
let timer = Unix.gettimeofday

let time f x =
  let t0 = timer () in
  let ret = f x in
  timer () -. t0, ret

let log_time =
  let timing_file = File.open_out "marg_like_timing.csv" in
  let ch = csv_out_channel timing_file |> Csv.to_out_obj in
  let f = Csv.output_record ch in
  f ["pquery";"pos";"component";"subtype";"step";"time"];
  f

let log_fit =
  let timing_file = File.open_out "marg_like_fit.csv" in
  let ch = csv_out_channel timing_file |> Csv.to_out_obj in
  let f = Csv.output_record ch in
  f ["pquery";"type";"pos";"dist_bl";"pend_bl";"ll";"fit_ll"];
  f

(* BSM for a pair of taxa connected by a branch of length t *)
module Pair = struct
  type bsm = {c: float; m: float; r: float; b: float}
  type point = (float * float)

  (* Run the LM algorithm to fit *)
  external fit: bsm -> point array -> bsm = "caml_lcfit_pair_fit"

  (* Find the ML branch length for the BSM model *)
  let ml_t model =
    (log((model.c -. model.m) /. (model.c +. model.m)) /. -. model.r) -. model.b

  (* Calculate the log-likelihood of branch-length `t` under `model` *)
  let log_like model t =
    let expterm = exp(-. model.r *. (t +. model.b)) in
    model.c *. log((1. +. expterm) /. 2.) +. model.m *. log((1.-.expterm) /. 2.)

  (* Scale factor for c and m to match observed likelihood `l` at time `t` *)
  let scale_factor t l model =
    l /. (log_like model t)

  (* Scale the BSM `m` to intersect with point (t, l) *)
  let rescale (t, l) m =
    let fac = scale_factor t l m in
    {m with c=m.c*.fac;m=m.m*.fac}

  let default_model =
    {c=1500.;m=1000.;r=1.0; b=0.5}

  type mononicity_class = Increasing | Decreasing | Non_monotonic | Unknown

  (* Classify a set of points ordered by x-value *)
  let rec monotonicity pts =
    match pts with
      | (t, ll) :: rest ->
          (let f ((_, last_ll), maybe_inc, maybe_dec) (t, ll) =
             if ll > last_ll then ((t, ll), maybe_inc, false)
             else if ll < last_ll then ((t, ll), false, maybe_dec)
             else ((t, ll), maybe_inc, maybe_dec)
           in
           let _, maybe_inc', maybe_dec' = List.fold_left f ((t, ll), true, true) rest
           in
           match (maybe_inc', maybe_dec') with
             | (false, false) -> Non_monotonic
             | (true, false) -> Increasing
             | (false, true) -> Decreasing
             | _ -> Unknown)
      | [] -> failwith "Points required"

  (* Given a likelihood function, select some points to run L-M on.
   *
   * Up to `max_pts` are added to the set until the points enclose an extremum.
   *
   * While the function is monotonically increasing, the maximum x-value is
   * doubled.
   *
   * While the function is monotonically decreasing, the minimum y-value is
   * halved.
   *)
  let select_points ?(max_points=8) log_like' pts =
    let rec aux pts =
      let n = DynArray.length pts
      and xi = (DynArray.get pts) |- fst in
      if n >= max_points then pts
      else
        let m = monotonicity (pts |> DynArray.enum |> List.of_enum) in
        let x, idx = match m with
          | Non_monotonic -> (((xi 1) +. (xi 2)) /. 2.0, 2)
          | Increasing -> ((xi (n - 1)) *. 2., n)
          | Decreasing -> ((xi 0) /. 10.0, 0)
          | Unknown -> failwith "Unknown monotonicity"
        in
        if m = Non_monotonic then pts
        else begin
          DynArray.insert pts idx (x, log_like' x);
          aux pts
        end
    in
    aux pts

  (* Choose points using `select_pts`, fit using top `pts_to_fit`. *)
  let find_points_fit_model
      ?(init_model=default_model)
      ?(pts_to_fit=4)
      ?(initial=[0.01; 0.1; 0.5; 1.0;])
      log_like' =
    (* Select up to max_points to fit *)
    let pts = List.enum initial
      |> Enum.map (fun x -> (x, log_like' x))
      |> DynArray.of_enum
      |> select_points log_like'
      |> DynArray.enum
      |> List.of_enum
      |> List.sort (fun (_, x) (_, y) -> compare y x)
      |> List.enum
      |> Enum.take pts_to_fit
      |> List.of_enum
    in
    let max_pt = List.hd pts in
    (rescale max_pt init_model
       |> (flip fit (Array.of_list pts)), pts)
      (*|> tap [> Log fits <]*)
          (*(fun (m, _) ->*)
            (*let log_like' = log_like m in*)
            (*List.map*)
              (*(fun (pend_bl, ll) -> [0.0; pend_bl; ll; log_like' pend_bl])*)
              (*pts*)
              (*|> List.enum*)
              (*|> map (List.map Float.to_string)*)
              (*|> map (List.append [!pquery_name;"pair";Int.to_string !pq])*)
              (*|> Enum.iter log_fit)*)

  let calc_marg_prob model prior base_ll upper_limit =
    let max_n_exceptions = 10
    and n_exceptions = ref 0
    and ll = log_like model
    and abs_err = 0.
    and rel_err = 1e-2
    in
    let rec perform upper_limit =
      if !n_exceptions >= max_n_exceptions then begin
        Printf.printf
          "Lcfit: integration did not converge after changing bounds %d times\n"
          max_n_exceptions;
        0.
      end
      else
        try
          Integration.value_integrate
            (fun t ->
              (exp ((ll t) -. base_ll)) *. (prior t))
            0. upper_limit ~abs_err ~rel_err
      |> log
      |> (+.) base_ll
        with
          | Gsl_error.Gsl_exn (Gsl_error.ETOL, _) ->
              incr n_exceptions;
              perform (upper_limit /. 2.)
    in
    perform upper_limit
end

(* BSM for a tripod, fitting the two-taxon model for each distal branch length *)
module TPair = struct
  module P = Pair
  let calc_marg_prob ~rel_err ~cut_bl ~upper_limit prior base_ll log_like =
    let abs_err = 0.
    and max_n_exceptions = 10
    and n_exceptions = ref 0
    and model = ref P.default_model
    and pts = ref [0.01; 0.1; 0.5; 1.0]
    in
    let rec perform upper_limit =
      if !n_exceptions >= max_n_exceptions then begin
        Printf.printf
          "Lcfit: integration did not converge after changing bounds %d times\n"
          max_n_exceptions;
        Printf.printf "cut_bl: %f; upper_limit: %f " cut_bl upper_limit;
        List.print Float.print stdout !pts;
        Printf.printf "\n";
        base_ll
      end
      else
        let inner_integration dist_bl =
          let pmax = List.max !pts in
          let init_pts = if pmax > upper_limit then
              List.map (( *. ) (upper_limit /. pmax)) !pts
            else
              !pts
          in
          let model', pts' = P.find_points_fit_model
            ~init_model:!model
            ~initial:init_pts
            (log_like dist_bl)
          in
          model := model';
          (* pts := List.map fst pts'; *)
          let log_like' = P.log_like model' in
          List.iter
            (fun (p, actual) -> log_fit [!pquery_name;
                                         "tpair";
                                         Int.to_string !pq;
                                         Float.to_string dist_bl;
                                         Float.to_string p;
                                         Float.to_string actual;
                                         Float.to_string (log_like' p)])
            pts';
          let f p = (exp ((log_like' p) -. base_ll)) *. (prior p) in
          Integration.value_integrate
            f
            0.
            upper_limit
            ~abs_err
            ~rel_err
        in
        let outer_integration () =
          (/.)
            (Integration.value_integrate
               inner_integration 0. cut_bl ~abs_err ~rel_err)
            cut_bl
        in
        try
          (if cut_bl =~ 0. then inner_integration (cut_bl /. 2.)
           else outer_integration ())
            |> log
            |> (+.) base_ll
        with
          | Gsl_error.Gsl_exn (Gsl_error.ETOL, _) ->
            (* Integration failed to reach tolerance with highest-order rule. Because
             * these functions are smooth, the problem is too-wide integration bounds.
             * We halve and try again. This is obviously pretty rough, but if we
             * aren't reaching tolerance then the posterior surface is dropping off
             * really fast compared to the size of the interval, so missing a little
             * of it is not going to make a difference. *)
            incr n_exceptions;
            perform (upper_limit /. 2.)
    in
    perform upper_limit
end

(* BSM for a tripod, with one branch length fixed (reference branch) *)
module Tripod = struct
  (* distal branch length, pendant branch length, log-likelihood *)
  type point = (float * float * float)

  (* The binary symmetric model for a tripod, with one branch length fixed *)
  type bsm = {n00: float; n01: float; n10: float; n11: float;
              r:   float; b:   float; t:   float; rx:  float;
              bx:  float}

  (* Evaluate the log-likelihood of the BSM given dist_bl, pend_bl, and the model *)
  external log_like: bsm -> float -> float -> float = "caml_lcfit_tripod_ll"

  (* Evaluate the jacobian of the BSM given dist_bl, pend_bl, and the model *)
  external jacobian: bsm -> float -> float -> float array = "caml_lcfit_tripod_jacobian"

  (* Fit the BSM given vectors of (dist_bl, pend_bl, log_like) and an initial estimate of the
     model *)
  external fit: bsm -> point array -> bsm = "caml_lcfit_tripod_fit"

  external est_rx: bsm -> point -> float = "caml_lcfit_tripod_est_rx"

  (* Rescale m to intersect with (dist_bl, pend_bl, ll) *)
  let rescale (dist_bl, pend_bl, ll) m =
    let est_ll = log_like m dist_bl pend_bl in
    let fac = ll /. est_ll in
    {m with n00=m.n00 *. fac;
      n01=m.n01 *. fac;
      n10=m.n10 *. fac;
      n11=m.n11 *. fac}

  (* Attempt to get rx on the correct order of magnitude for
   * (dist_bl, pend_bl, ll) *)
  let rescale_rx (dist_bl, pend_bl, ll) m =
    let rec aux m =
      let fit_ll = log_like m dist_bl pend_bl in
      let m' =
        if fit_ll < ll then {m with rx=m.rx *. 0.1}
        else {m with rx=m.rx /. 10.}
      in
      if Float.abs (ll -. (log_like m' dist_bl pend_bl)) < Float.abs (ll -. fit_ll)
      then aux m'
      else m
    in
    aux m

  (* This seems to provide sensible defaults *)
  let default_model t =
    {n00=1500.;n01=300.;n10=300.;n11=300.;r=1.;b=0.5;t=t;rx=1.;bx=0.5}

  (* Remove any points from `pts` within `min_dist` of an earlier point. *)
  (* TODO: something smarter here? O(n^2) *)
  let remove_near_neighbors ?(min_dist=1e-4) pts =
    let sq = flip Float.pow 2.0 in
    let sq_min = sq min_dist in
    let any_close kept (x, y, _) =
      List.enum kept
                                           |> Enum.map (fun (x', y', _) -> (sq (x -. x')) +. (sq (y -. y')))
                                           |> Enum.exists (fun i -> i < sq_min)
    in
    let rec aux kept = function
      | [] -> kept
      | pt :: rest ->
          if any_close kept pt then
            aux kept rest
          else
            aux (pt :: kept) rest
    in
    aux [] pts
      (*|> tap (fun r ->*)
          (*dprintf "Pruned from %d to %d points.\n"*)
            (*(List.length pts)*)
            (*(List.length r))*)

  (* Evaluate log_like' at the cartesian product of points sampled uniformly
   * between [0,cut_bl] and [0,max_pend], fit the tripod_bsm using these points
   * and additional points in `pts` *)
  let find_points_fit_model ?(max_pend=2.0) cut_bl log_like' pts =
    let lattice n =
      let tuple2_extend f (x, y) = (x, y, f x y) in
      let seq min max =
        let d = max -. min
        and nf = (Float.of_int n) -. 1.0 in
        assert(d > 0.);
        0 -- (n - 1)
                                           |> map Float.of_int
                                           |> map (fun i -> min +. (d *.i) /. nf)
                                           |> List.of_enum
      in
      List.cartesian_product (seq 1e-5 (cut_bl -. 1e-5)) (seq 1e-5 max_pend)
                                           |> List.map (tuple2_extend log_like')
    and drop_low_ll ?(ll_diff=5.0) pts =
      let max_ll = List.enum pts
        |> Enum.map Tuple3.third
        |> Enum.arg_max identity
      in
      List.filter (fun i -> (Tuple3.third i) >= max_ll -. ll_diff) pts
    in
    let lattice_pts = lattice 10 in
    let lattice_pts = lattice 10 in
    let pts' = remove_near_neighbors pts
                                           |> List.append lattice_pts
                                           |> drop_low_ll
    and max_ll = List.hd pts in
    let init_model = default_model cut_bl |> rescale max_ll in
    let fit_time, model = time (fit init_model) (Array.of_list pts') in
      log_time [!pquery_name; Int.to_string !pq; "lcfit"; "tripod"; "fit_model"; Float.to_string fit_time];
      model
    (* End debugging *)

    let calc_marg_prob model cut_bl prior base_ll upper_limit =
    (* Select some points to sample - uniformly from 0-cut_bl, 0-max_pend *)
      let max_n_exceptions = 10
      and n_exceptions = ref 0
      and ll = log_like model
      and abs_err = 0.
      and rel_err = 1e-2
      in
      let rec perform upper_limit =
        if !n_exceptions >= max_n_exceptions then begin
          Printf.printf
            "Lcfit: integration did not converge after changing bounds %d times\n"
            max_n_exceptions;
          base_ll
        end
        else
          let inner_integration dist_bl =
            Integration.value_integrate
              (fun pend_bl ->
                (exp ((ll dist_bl pend_bl) -. base_ll)) *. (prior pend_bl))
              0. upper_limit ~abs_err ~rel_err
          in
          let outer_integration () =
            (/.)
              (Integration.value_integrate
                 inner_integration
                 0. cut_bl ~abs_err ~rel_err)
              cut_bl
          in
          try
            (if cut_bl =~ 0.
             then inner_integration (cut_bl /. 2.)
             else outer_integration ())
                                           |> log
                                           |> (+.) base_ll
          with
            | Gsl_error.Gsl_exn (Gsl_error.ETOL, _) ->
                incr n_exceptions;
                perform (upper_limit /. 2.)
      in perform upper_limit
end
