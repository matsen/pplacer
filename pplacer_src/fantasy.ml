(* pplacer v1.0. Copyright (C) 2009-2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 * routines for "fantasy baseball"
*)

open Fam_batteries

type info = { n_hits : int; n_trials : int; like_diff : float }

let empty_info = { n_hits = 0; n_trials = 0; like_diff = 0.; }

let get_n_hits r = r.n_hits
let get_n_trials r = r.n_trials
let get_like_diff r = r.like_diff

let add_to_info a (hit, n_trials, like_diff) = 
  { n_hits = a.n_hits + if hit then 1 else 0; 
  n_trials = a.n_trials + n_trials; 
  like_diff = a.like_diff +. like_diff; }

let get_like (_, (like, _, _)) = like
let get_loc (loc, _) = loc

let find_best ml_results = 
  let rec aux best_loc like_record = function
    | r::rest ->
        if get_like r > like_record then 
          aux (get_loc r) (get_like r) rest
        else
          aux best_loc like_record rest
    | [] -> (best_loc, like_record)
  in
  match ml_results with
  | r::rest -> aux (get_loc r) (get_like r) rest
  | [] -> assert(false)


(* returns (hit, n_trials). hit is whether we would have gotten a hit (i.e. if
 * we found the best one by playing baseball) and the number of trials needed.
 * note that we don't do anything with max_pitches, assuming that is has been
 * taken care of by filtering down the ml_results. *)
let fantasy_ball ml_results strike_box max_strikes = 
  let (best_loc, best_like) = find_best ml_results in
  let rec play_ball our_like_record n_strikes prev_found prev_n_trials = function
    | (loc, (like,_,_))::rest -> begin
      let n_trials = prev_n_trials+1 in
      (* have we found it yet? *)
      let found = if loc = best_loc then true else prev_found in
      if like > our_like_record then
        (* we have a new best likelihood *)
        play_ball like n_strikes found n_trials rest 
      else if like < our_like_record-.strike_box then
        (* we have a strike *)
        if n_strikes+1 >= max_strikes then 
        (* struck out *)
          (found, n_trials, our_like_record)
        else play_ball our_like_record (n_strikes+1) found n_trials rest 
      else
        (* not a strike, just keep on accumulating results *)
        play_ball our_like_record n_strikes found n_trials rest
       end
    | [] -> (prev_found, prev_n_trials, our_like_record)
  in
  let (found, n_trials, our_like_record) = 
    play_ball (-. infinity) 0 false 0 ml_results in
  (found, n_trials, best_like -. our_like_record)

let make_fantasy_matrix ~max_strike_box ~max_strikes = 
  Array.make_matrix (1+max_strike_box) (1+max_strikes) empty_info

let add_single_to_fm fm strike_box max_strikes result = 
  fm.(strike_box).(max_strikes) <- 
    add_to_info (fm.(strike_box).(max_strikes)) result

let add_to_fantasy_matrix ml_results fm = 
  let max_strike_box = (Array.length fm)-1
  and max_strikes = (Array.length fm.(0))-1
  in
  for strike_box=0 to max_strike_box do
    for max_strikes=0 to max_strikes do
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

let calc_stats fantasy_mat n_fantasies = 
  let avg x = x /. (float_of_int n_fantasies) in
  (mat_map (fun r -> avg (float_of_int (get_n_hits r))) fantasy_mat,
   mat_map (fun r -> avg (float_of_int (get_n_trials r))) fantasy_mat,
   mat_map (fun r -> avg (get_like_diff r)) fantasy_mat)

(* find the set of parameters which minimizes the number of trials for a given
 * cutoff. the cutoff is the average likelihood difference for the fantasy
 * runs. return Some (strike box, n strikes) if found, None if not.  *)
let find_optimum fantasy_mat cutoff n_fantasies = 
  let best_choice = ref (-1,-1)
  and best_n_trials = ref max_int
  (* the cutoff argument is an average *)
  and big_cutoff = cutoff *. (float_of_int n_fantasies)
  in
  MatrixFuns.iterij
    (fun sbox maxs info -> 
      let our_trials = get_n_trials info in
      if (get_like_diff info) < big_cutoff && 
                   our_trials < !best_n_trials then begin
        best_choice := (sbox, maxs);
        best_n_trials := our_trials
      end)
    fantasy_mat;
  if !best_n_trials = max_int then None (* couldn't find anything *)
  else Some (!best_choice)

let print_optimum fantasy_mat cutoff n_fantasies = 
  match find_optimum fantasy_mat cutoff n_fantasies with
  | None -> 
      Printf.printf 
        "No (strike box, max strikes) combination found with average likelihood difference of %g or better.\n"
        cutoff
  | Some (strike_box, n_strikes) ->
      Printf.printf "Fantasy baseball results: fastest combination with average likelihood difference %g is as follows:\n" cutoff;
      Printf.printf "--strikeBox %d --maxStrikes %d\n" strike_box n_strikes

let arr_forget_first a = 
  let len = Array.length a in
  assert(len > 0);
  Array.sub a 1 (len-1)

let results_to_file fname_prefix fantasy_mat n_fantasies =
  let batting_avg,n_trials_avg,like_diff_avg = 
    calc_stats fantasy_mat n_fantasies 
  in
  let write_mat fname m = 
    let ch = open_out fname in
    Printf.fprintf ch "# fantasy run. results obtained by evaluating %d equally spaced sequences.\n" n_fantasies;
    Printf.fprintf ch "# strike box is first coordinate (indexed from zero), and max strikes is second (indexed from one).\n";
    String_matrix.write_padded ch 
      (mat_map string_of_float (Array.map arr_forget_first m));
    close_out ch
  in
  write_mat (fname_prefix^".batting_avg.out") batting_avg;
  write_mat (fname_prefix^".n_trials.out") n_trials_avg;
  write_mat (fname_prefix^".like_diff.out") like_diff_avg;
  ()

