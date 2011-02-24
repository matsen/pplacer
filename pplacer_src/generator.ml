
exception Last

class virtual ['a] generator =
  object
    method virtual next: 'a
  end

(* return a new object which is the previous with a filter applied *)
let filter f g =
  object
    method next =
      let rec aux () =
        let x = g#next in
        if f x then x
        else aux ()
      in
      aux ()
  end

let to_list g =
  let rec aux accu =
    try aux ((g#next_sr)::accu) with
    | Last -> accu
  in
  aux []



