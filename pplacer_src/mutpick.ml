
open MapsSets

let array_maxi a =
  let best = ref 0.
  and besti = ref (-1)
  in
  for i=0 to (Array.length a)-1 do
    if a.(i) > !best then begin
      best := a.(i);
      besti := i;
    end
  done;
  assert(-1 <> !besti);
  !besti

(* pick the ML state by taking the sum across rates for each state and site *)
let pick_of_rpost g =
  let n_sites = Glv.get_n_sites g
  and n_states = Glv.get_n_states g
  and n_rates = Glv.get_n_rates g
  in
  let picks = Array.make n_sites (-1)
  and u = Array.make n_states 0.
  in
  for site=0 to n_sites-1 do
    Array.fill u 0 n_states 0.;
    for rate=0 to n_rates-1 do
      for state=0 to n_states-1 do
        u.(state) <- u.(state) +. (Glv.get_a ~rate ~site ~state g)
      done
    done;
    picks.(site) <- array_maxi u
  done;
  picks

(* Get the most likely vector at a chosen node id.
 * atarr is the Glv_arr which gives the Glv on the side we are interested in,
 * while neigharr is the neighbor.
 * u1 and u2 are utility vectors like Glv.mimic (Glv_arr.get_one darr) *)
let get_pick u1 u2 model t ~atarr ~neigharr id =
  Glv.evolve_into model
    ~dst:u1
    ~src:(Glv_arr.arr_get neigharr id)
    (Gtree.get_bl t id);
  Glv.statd_pairwise_prod
    model ~dst:u2 u1 (Glv_arr.arr_get atarr id);
  pick_of_rpost u2

let get_pickpair u1 u2 model t ~darr ~parr id =
  let at_d = get_pick u1 u2 model t ~atarr:darr ~neigharr:parr id
  and at_p = get_pick u1 u2 model t ~atarr:parr ~neigharr:darr id
  in
  (at_d, at_p)

(* make a map from a list of edge ids to the most likely vectors on either side
 * of the edge: order is (distal, proximal) *)
let pickpair_map model t ~darr ~parr ids =
  let u1 = Glv.mimic (Glv_arr.get_one darr)
  and u2 = Glv.mimic (Glv_arr.get_one darr) in
  List.fold_right
    (fun id ->
      IntMap.add id (get_pickpair u1 u2 model t ~darr ~parr id))
    ids
    IntMap.empty
