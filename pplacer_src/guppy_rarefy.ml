open Subcommand
open Guppy_cmdobjs
open Ppatteries

let rarefy sample pqa =
  let multi_arr = Array.range pqa
    |> Enum.map
        (fun i ->
          Pquery.namlom pqa.(i)
            |> List.enum
            |> Enum.map (const i &&& identity))
    |> Enum.flatten
    |> Array.of_enum
  in
  let p = Array.map (snd %> snd) multi_arr in
  sample ~p
    |> Array.fold_lefti
        (fun accum j -> function
         | 0 -> accum
         | count ->
           let i, (name, _) = multi_arr.(j) in
           IntMap.add_listly i (name, float_of_int count) accum)
        IntMap.empty
    |> IntMap.enum
    |> Enum.fold
        (fun accum (i, namlom) -> Pquery.set_namlom pqa.(i) namlom :: accum)
        []

let verify_inty f =
  if not (mod_float f 1. =~ 0.) then
    failwith "--weight-as-count requires integer counts on pqueries"

let multivariate_hypergeometric_sample rng ~n ~p =
  Array.iter verify_inty p;
  let src = Array.map round p in
  let sum = Array.reduce (+) src in
  if n > sum then
    failwith (Printf.sprintf "can't sample %d elements out of %d" n sum);
  let src = Array.map float_of_int src in
  let dst = Array.map (const 0) p in
  0 --^ n
  |> Enum.iter (fun _ ->
    let i = Gsl_randist.discrete rng (Gsl_randist.discrete_preproc src) in
    src.(i) <- src.(i) -. 1.; dst.(i) <- succ dst.(i));
  dst

class cmd () =
object (self)
  inherit subcommand () as super
  inherit placefile_cmd () as super_placefile
  inherit output_cmd () as super_output
  inherit rng_cmd () as super_rng

  val n_taken = flag "-n"
    (Needs_argument ("n_taken", "The number of pqueries to keep per placefile."))
  val weight_as_count = flag "--weight-as-count"
    (Plain (false, "Interpret (integer) weights on pqueries as counts and sample without replacement."))

  method specl =
    super_output#specl
  @ super_rng#specl
  @ [
    int_flag n_taken;
    toggle_flag weight_as_count;
  ]

  method desc = "performs rarefaction on collections of placements"
  method usage = "usage: rarefy [options] placefile"

  method private placefile_action prl =
    let sample_fun =
      if fv weight_as_count then multivariate_hypergeometric_sample
      else Gsl_randist.multinomial
    in
    let sample = sample_fun self#rng ~n:(fv n_taken)
    and gt = Mokaphy_common.list_get_same_tree prl in
    List.map
      (Placerun.get_pqueries %> Array.of_list %> rarefy sample)
      prl
    |> List.flatten
    |> Placerun.make gt ""
    |> self#write_placefile (self#single_file ())

end
