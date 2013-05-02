open Ppatteries

type 'a t = 'a option ref

let create () = ref None

let init v = ref (Some v)

let of_option = ref

let reify v f = match !v with
  | Some v' -> v'
  | None ->
    let v' = f () in
    v := Some v';
    v'
