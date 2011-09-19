open Subcommand
open Guppy_cmdobjs

let multiply_list l =
  let rec aux subl = function
    | 0 -> []
    | 1 -> subl
    | n when n > 1 ->
      aux (List.rev_append l subl) (n - 1)
    | _ -> invalid_arg "multiply_list"
  in
  aux l

class cmd () =
object (self)
  inherit subcommand () as super
  inherit placefile_cmd () as super_placefile
  inherit output_cmd () as super_output

  val mult = flag "-x"
    (Plain ([], "Apply a multiplier to the given placefile, e.g. -x 2:y.json"))

  method specl = super_output#specl @ [
    string_list_flag mult;
  ]

  method desc = "merges placefiles together"
  method usage = "usage: merge [options] placefiles"

  method private placefile_action prl =
    let prl = List.rev_append
      (List.map
         (fun s ->
           let mult, prn = Scanf.sscanf s "%d:%s" (fun x y -> x, y) in
           let pr = Placerun_io.of_any_file prn in
           Placerun.set_pqueries
             pr
             (List.map
                (fun pq ->
                  Pquery.set_namel
                    pq
                    (multiply_list (Pquery.namel pq) mult))
                (Placerun.get_pqueries pr)))
         (fv mult))
      prl
    in match prl with
    | [] -> ()
    | prl ->
      let fname = self#single_file
        ~default:(File ((Mokaphy_common.cat_names prl) ^ ".jplace"))
        ()
      in
      let combined = List.fold_left
        (Placerun.combine "")
        (List.hd prl)
        (List.tl prl)
      in
      self#write_placefile "guppy merge" fname combined
end
