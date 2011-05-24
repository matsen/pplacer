(* The algorithm is simple: first, with a two pass recursion, find the mark
 * label and distance to the closest leaf for all of the internal nodes. Now say
 * we are on an edge of length l bounded by internal nodes i_d (distal) and i_p
 * (proximal). If the mark label is the same for each of i_d and i_p, then there
 * are no marks inside the given edge. OTOH, if they are different and the
 * distances to the closest leaves are d_d and d_p, respectively, then we should
 * put a mark at (l + d_d - d_p)/2 with label taken from i_d.
 *)
