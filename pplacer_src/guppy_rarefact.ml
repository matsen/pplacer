open Subcommand
open Guppy_cmdobjs
open Ppatteries

class cmd () =
object (self)
  inherit subcommand () as super
  inherit mass_cmd ~point_choice_allowed:false () as super_mass
  inherit placefile_cmd () as super_placefile
  inherit tabular_cmd () as super_tabular

  val variance = flag "--variance"
    (Plain (false, "Calculate variance of phylogenetic entropy."))
  val weight_as_count = flag "--weight-as-count"
    (Plain (false, "Interpret (integer) weights on pqueries as counts."))
  val k_max = flag "-k"
    (Needs_argument ("k max", "The highest value of k to calculate."))

  method specl =
    super_mass#specl
  @ super_tabular#specl
  @ [
    toggle_flag variance;
    toggle_flag weight_as_count;
    int_flag k_max;
  ]

  method desc = "calculates phylogenetic rarefaction curves"
  method usage = "usage: rarefact [options] placefile"

  method private placefile_action = function
    | [pr] ->
      let k_max = fvo k_max in
      let pr =
        if fv weight_as_count then Placerun.duplicate_pqueries_by_count pr
        else pr
      in
      let criterion = self#criterion in
      let is_uniform_mass =
        Placerun.get_pqueries pr
        |> List.map (Pquery.namlom |- List.map snd)
        |> List.flatten
        |> List.sort_unique (<~>)
        |> List.length
        |> (=) 1
      and fmt = Printf.sprintf "%g" in
      if not is_uniform_mass then begin
        if fv variance then
          failwith "not all sequences have uniform weight; variance can't be \
                    calculated";
        deprint "warning: not all sequences have uniform weight; expectation \
                 of quadratic entropy can't be calculated\n"
      end;
      Rarefaction.of_placerun criterion ?k_max pr
      |> Enum.map
          (fun (k, um, rm, qm) ->
            [string_of_int k; fmt um; fmt rm]
            @ (if is_uniform_mass then [fmt qm] else []))
      |> begin
        if fv variance then
          curry
            Enum.combine
            (Rarefaction.variance_of_placerun criterion ?k_max pr)
          |- Enum.map (fun ((_, uv, rv), sl) -> sl @ [fmt uv; fmt rv])
        else identity
      end
      |> List.of_enum
      |> List.cons
          (["k"; "unrooted_mean"; "rooted_mean"]
           @ (if is_uniform_mass then ["quadratic_mean"] else [])
           @ (if fv variance then ["unrooted_variance"; "rooted_variance"] else []))
      |> self#write_ll_tab

    | l ->
      List.length l
      |> Printf.sprintf "rarefact takes exactly one placefile (%d given)"
      |> failwith

end
