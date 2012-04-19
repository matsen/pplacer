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
  let p = Array.map (snd |- snd) multi_arr in
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

class cmd () =
object (self)
  inherit subcommand () as super
  inherit placefile_cmd () as super_placefile
  inherit output_cmd () as super_output
  inherit rng_cmd () as super_rng

  val n_taken = flag "-n"
    (Needs_argument ("n_taken", "The number of pqueries to keep per placefile."))

  method specl =
    super_output#specl
  @ super_rng#specl
  @ [int_flag n_taken]

  method desc = "performs rarefaction on collections of placements"
  method usage = "usage: rarefy [options] placefile"

  method private placefile_action prl =
    let sample = Gsl_randist.multinomial self#rng ~n:(fv n_taken)
    and gt = Mokaphy_common.list_get_same_tree prl in
    List.map
      (Placerun.get_pqueries |- Array.of_list |- rarefy sample)
      prl
    |> List.flatten
    |> Placerun.make gt ""
    |> self#write_placefile (self#single_file ())

end
