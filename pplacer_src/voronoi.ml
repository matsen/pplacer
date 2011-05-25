
let mark_min m1 m2 = if (fst m1) <= (fst m2) then m1 else m2

let distal_mark_map t =
  (* recursion over tree, getting the (distance, color) from each internal node
   * to its closest /distal/ leaf.
   * *)

let internal_mark_map t =
  (* second recursion to find the (distance, color) from each internal node to
   * its closest leaf. At each internal node, simply keep track of the smallest
   * distance coming from above (can do options but I do think that seeding with
   * infinity and an empty string should be fine here, as long as we check to
   * make sure the eventual float is finite.)
   * *)

let update v above_cdist coptim =
  (* Go down until we hit a mark or a truly colored leaf, then proceed back up
   * the tree.
   * *)

let of_gtree t =
  (* The algorithm is simple: first, with a two pass recursion, find the mark
   * color and distance to the closest leaf for all of the internal nodes. Now
   * say we are on an edge of length l bounded by internal nodes i_d (distal)
   * and i_p (proximal). If the mark color is the same for each of i_d and i_p,
   * then there are no marks inside the given edge. OTOH, if they are different
   * and the distances to the closest leaves are d_d and d_p, respectively, then
   * we should put a mark at (l + d_d - d_p)/2 with color taken from i_d.
 *)

let remove_color v c =
  (* We recur through the tree *)
