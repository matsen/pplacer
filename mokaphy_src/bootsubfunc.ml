(* mokaphy v1.0. Copyright (C) 2010  Frederick A Matsen.
 * This file is part of mokaphy. mokaphy is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 * Say we bootstrap a clustering method. 
 * If there are samples that don't fit into a tree-like structure, then they
 * will wander about and make all of the bootstrap values low.
 * We would like to find them and look at bootstrap values when those taxa are
 * removed.
 *
 * Note that we are on very thin statistical ice here: once we start monkeying
 * around with the sets, the bootstrap instantly loses its usual interpretation.
 *
 * Bootsub is a functorial module interface for finding these wandering subsets
 * of clusters.
 *
 * The main function of this interface is perform, which takes a desired cutoff
 * value for the bootstrap, and a set for which we would like to "improve" the
 * bootstrap value, and then the list of set sets, which are the bootstrapped
 * clusters.
 *
 * Say s is the set in question, and ssl is our bootstrap set of sets.
 * The algorithm is as follows (it's greedy, and has no guarantees of globally
 * optimal performance):
 *   - for each ss in ssl, find the smallest symmetric difference between s and
 *   a set in ss
 *   - find the most popular element in those symmetric differences. this
 *   element is called "naughty" and is the one that jumps around a lot
 *   - remove this element from every set in ssl, and recur.
 *)

module Make 
         (O: Set.OrderedType) 
         (S: Set.S with type elt = O.t)
         (SS: Set.S with type elt = S.t) = struct

  (* madness *)
  exception First of O.t
  let first_key h = 
    try Hashtbl.iter (fun k _ -> raise (First k)) h; raise Not_found with 
    | First k -> k

  let s_of_list l = List.fold_right S.add l S.empty

  (*
   * # (x,y);;
  * - : MapsSets.StringSet.t * MapsSets.StringSet.t = ({1; 2; 3}, {3; 4})
  * # let r = symmdiff x y;;
  * val r : MapsSets.StringSet.t = {1; 2; 4}
  *)
  let symmdiff s s' = S.union (S.diff s s') (S.diff s' s)

  (* f is a map from elements of list to some comparable quantities *)
  let find_fsmallest f startl = 
    let rec aux x v = function
      | x'::l ->
          let v' = f x' in
          if v' < v then aux x' v' l
          else aux x v l
      | [] -> x
    in
    match startl with
    | [] -> assert false
    | h::t -> aux h (f h) t 
  
  (* given a StringSetSet, find the StringSet with the smallest symmetric
   * difference with chosen_s *)
  let smallest_symmdiff s ss = 
    find_fsmallest S.cardinal (List.map (symmdiff s) (SS.elements ss))
  
  (* given a list of S.t's, most_pop finds the element which is most
   * commonly seen *)
  let most_pop sl = 
    let h = Hashtbl.create ((S.cardinal (List.hd sl)) / 4) in
    let boost x = 
      if Hashtbl.mem h x then Hashtbl.replace h x (1 + (Hashtbl.find h x))
      else Hashtbl.add h x 1
    in
    List.iter (S.iter boost) sl;
    let fk = first_key h in
    Hashtbl.fold 
      (fun k v ((_, bv) as p) -> if v > bv then (k, v) else p) 
      h
      (fk, Hashtbl.find h fk)
  
  (* remove x from every elememnt of ss *)
  let ssremove x ss = 
    SS.fold
      (fun s -> SS.add (if S.mem x s then S.remove x s else s)) ss SS.empty
  
  (* See the top of this code for the full introduction to this function.
   *
   * the score is the fraction of the SSets in our list will contain a set
   * identical to start_s upon removal of the naughty elements. 
   * we use this funny aux and bump_down design because it's expensive to
   * compute the score, and in doing so we make symmdiffl, which is then used to
   * find the next naughty.
   * *)
  let perform cutoff start_s start_ssl = 
   (* the final return of aux is a pair (final_score, l) where l's elements are
    * of the form (s,x), where s is the score before removing x *)
    let inv_listnl = 1. /. (float_of_int (List.length start_ssl)) in
    let rec aux s ssl accu =
      let symdiffl = List.map (smallest_symmdiff s) ssl in
      (* score is the score before deletion *)
      let score = 
        List.fold_right 
          (fun s -> (+.) (if s = S.empty then inv_listnl else 0.))
          symdiffl
          0.
      in
      if score >= cutoff then 
        score, accu
      else begin
        let (naughty,_) = most_pop symdiffl in
          aux
            (S.remove naughty s)
            (List.map (ssremove naughty) ssl)
            ((score, naughty)::accu)
      end
    in
    (* then we have to bump them down, so the score is paired with the element
     * whose removal gives that score *)
    let rec bump_down next_score = function
      | [] -> [next_score, None]
      | (score, naughty)::l -> (next_score, Some naughty)::(bump_down score l)
    in
    let (final_score, l) = aux start_s start_ssl [] in
    bump_down final_score l
end
