
exception Last

class virtual seq_generator =
  object
    method virtual next_sr: Seq_record.sr
  end

(* return a new object which is the previous with a filter applied *)
let filter f sg =
  object
    method next_sr =
      let rec aux () =
        let sr = sg#next_sr in
        if f sr then sr
        else aux ()
      in
      aux ()
  end

let to_list sg =
  let rec aux accu =
    try aux ((sg#next_sr)::accu) with
    | Last -> accu
  in
  aux []



