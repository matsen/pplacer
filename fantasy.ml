(* pplacer v0.3. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 * routines for "fantasy baseball"

 * heat map:
   * http://sekhon.berkeley.edu/graphics/html/image.html
*)


let get_like (_, (like, _, _)) = like
let get_loc (loc, _) = loc

let find_best_loc ml_results = 
  let rec aux best_loc like_record = function
    | r::rest ->
        if get_like r > like_record then 
          aux (get_loc r) (get_like r) rest
        else
          aux best_loc like_record rest
    | [] -> best_loc
  in
  match ml_results with
  | r::rest -> aux (get_loc r) (get_like r) rest
  | [] -> assert(false)


(* returns (hit, n_trials). hit is whether we would have gotten a hit (i.e. if
 * we found the best one by playing baseball) and the number of trials needed.
 * note that we don't do anything with max_pitches, assuming that is has been
 * taken care of by filtering down the ml_results. *)
let fantasy_ball ml_results strike_box max_strikes = 
  let best_loc = find_best_loc ml_results in
  let rec play_ball like_record n_strikes prev_found prev_n_trials = function
    | (loc, (best_like,_,_))::rest -> begin
      let n_trials = prev_n_trials+1 in
      (* have we found it yet? *)
      let found = if loc = best_loc then true else prev_found in
      if best_like > like_record then
        (* we have a new best likelihood *)
        play_ball best_like n_strikes found n_trials rest 
      else if best_like < like_record-.strike_box then
        (* we have a strike *)
        if n_strikes+1 >= max_strikes then 
        (* struck out *)
          (found, n_trials)
        else play_ball like_record (n_strikes+1) found n_trials rest 
      else
        (* not a strike, just keep on accumulating results *)
        play_ball like_record n_strikes found n_trials rest
       end
    | [] -> (prev_found, prev_n_trials)
  in
  play_ball (-. infinity) 0 false 0 ml_results

let make_fantasy_matrix ~max_strike_box ~max_strikes = 
  Array.make_matrix (1+max_strike_box) (1+max_strikes) (0,0)

let add_single_to_fm fm strike_box max_strikes (hit, n_trials) = 
  let (ph, pnt) = fm.(strike_box).(max_strikes) in
  fm.(strike_box).(max_strikes) <- 
    (ph + (if hit then 1 else 0), pnt + n_trials)

let add_to_fantasy_matrix ml_results fm = 
  let max_strike_box = (Array.length fm)-1
  and max_strikes = (Array.length fm.(0))-1
  in
  for strike_box=0 to max_strike_box do
    for max_strikes=1 to max_strikes do
      add_single_to_fm fm strike_box max_strikes 
        (fantasy_ball ml_results (float_of_int strike_box) max_strikes)
    done;
  done

(* mostly for testing.. we will typically make one fm then add lots of seqs to
 * it. *)
let build_fantasy_matrix ml_results ~max_strike_box ~max_strikes = 
  let fm = make_fantasy_matrix max_strike_box max_strikes in
  add_to_fantasy_matrix ml_results fm;
  fm

let mat_map f m = Array.map (Array.map f) m

let calc_stats fantasy_mat num_queries = 
  let avg x = (float_of_int x) /. (float_of_int num_queries) in
  let get_stat f = mat_map (fun r -> avg (f r)) fantasy_mat in
  (get_stat fst, get_stat snd)

let results_to_file fname_prefix fantasy_mat num_queries =
  let batting_avg,n_trials_avg = calc_stats fantasy_mat num_queries in
  let write_mat fname m = 
    let ch = open_out fname in
    Printf.fprintf ch "# strike box is first coordinate, and max strikes is second.\n";
    String_matrix.write_padded ch (mat_map string_of_float m);
    close_out ch
  in
  write_mat (fname_prefix^".fantasy.ba.out") batting_avg;
  write_mat (fname_prefix^".fantasy.nt.out") n_trials_avg;
  ()
