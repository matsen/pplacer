open MapsSets

(* pick the ML state by taking the sum across rates for each state and site *)
let summarize_post summarize_f initial g =
  let n_sites = Glv.get_n_sites g
  and n_states = Glv.get_n_states g
  and n_rates = Glv.get_n_rates g
  in
  let summary = Array.make n_sites initial
  and u = Gsl_vector.create ~init:0. n_states
  in
  for site=0 to n_sites-1 do
    Gsl_vector.set_all u 0.;
    for rate=0 to n_rates-1 do
      for state=0 to n_states-1 do
        u.{state} <- u.{state} +. (Glv.get_a ~rate ~site ~state g)
      done
    done;
    summary.(site) <- summarize_f u
  done;
  summary

(* Get the most likely vector at a chosen node id.
 * atarr is the Glv_arr which gives the Glv on the side we are interested in,
 * while neigharr is the neighbor.
 * u1 and u2 are utility vectors like Glv.mimic (Glv_arr.get_one darr) *)
let get_posterior ~dst util model t ~atarr ~neigharr id =
  Glv.evolve_into model
    ~dst:util
    ~src:(Glv_arr.arr_get neigharr id)
    (Gtree.get_bl t id);
  Glv.statd_pairwise_prod
    model ~dst util (Glv_arr.arr_get atarr id)

type pos = Distal | Proximal

let get_summary pos summarize_f initial u1 u2 model t ~darr ~parr id =
  let (atarr, neigharr) = match pos with
    | Distal -> (darr, parr)
    | Proximal -> (parr, darr)
  in
  get_posterior ~dst:u1 u2 model t ~atarr ~neigharr id;
  summarize_post summarize_f initial u1

(* make a map from a list of edge ids to the most likely vectors on either side
 * of the edge: order is (distal, proximal) *)
let pickpair_map summarize_f initial model t ~darr ~parr ids =
  let u1 = Glv.mimic (Glv_arr.get_one darr)
  and u2 = Glv.mimic (Glv_arr.get_one darr) in
  List.fold_right
    (fun id ->
      IntMap.add
        id
        (get_summary Distal summarize_f initial u1 u2 model t ~darr ~parr id,
         get_summary Proximal summarize_f initial u1 u2 model t ~darr ~parr id))
    ids
    IntMap.empty
