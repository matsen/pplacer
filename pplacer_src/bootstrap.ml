open Ppatteries

(* Bootstrap a placerun. This has gotten a little tricky because pqueries
 * indicate their multiplicity by namloms, which are (string * float) lists.
 * The total multiplicity for a placerun is defined to be the total of all of
 * the floats. These pqueries will lose some of their names under
 * bootstrapping; e.g. [a, 4.; b,3.] may become [a, 9.].
  *)
let boot_placerun rng pr =
  let pqa = Array.of_list (Placerun.get_pqueries pr) in
  let multip = Array.map Pquery.multiplicity pqa in
  (* multa has the resampled multiplicity of each of the pqueries. *)
  let multa = Gsl.Randist.multinomial
    rng
    (int_of_float (Array.fold_left ( +. ) 0. multip)) (* get total multiplicity *)
    multip
  and pql = ref []
  in
  for i=(Array.length pqa)-1 downto 0 do
    if multa.(i) > 0 then
      let nl = [Pquery.name pqa.(i), float_of_int multa.(i)] in
      pql := {pqa.(i) with Pquery.namlom = nl}::(!pql)
  done;
  let new_pr = { pr with Placerun.pqueries = !pql } in
  assert(int_of_float (Placerun.total_multiplicity pr)
    = int_of_float (Placerun.total_multiplicity new_pr));
  new_pr

