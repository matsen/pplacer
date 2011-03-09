external quiet_close: int -> unit = "quiet_close"
external fd_of_file_descr: Unix.file_descr -> int = "%identity"

let range n =
  let rec aux accum n =
    if n = 0 then
      accum
    else
      aux ((n - 1) :: accum) (pred n)
  in aux [] n

exception Closed
let read_exactly fd n =
  let s = String.create n in
  let rec aux n_read =
    if n_read = n then s
    else begin
      let read = Unix.read fd s n_read (n - n_read) in
      if read = 0 then raise Closed
      else aux (n_read + read)
    end
  in aux 0

let make_child child_func =
  let rd, wr = Unix.pipe () in
  match Unix.fork () with
    | 0 ->
      let wr' = fd_of_file_descr wr in
      List.iter
        (fun fd -> if fd != wr' then quiet_close fd)
        (range 256);
      let wrc = Unix.out_channel_of_descr wr in
      child_func (fun x -> Marshal.to_channel wrc x []);
      exit 0
    | pid ->
      Unix.close wr;
      pid, rd

let child_loop handle_func =
  Sys.set_signal Sys.sigchld Sys.Signal_ignore;
  let rec aux pipes =
    let rr, _, _ = Unix.select pipes [] [] 10.
    in
    let closed = List.fold_left
      (fun cl p ->
        match begin
          try
            let header = read_exactly p 20 in
            let data = read_exactly p (Marshal.data_size header 0) in
            Some (header ^ data)
          with
            | Closed -> Unix.close p; None
        end with
          | Some data -> handle_func (Marshal.from_string data 0); cl
          | None -> p :: cl)
      []
      rr
    in
    match List.filter (fun p -> not (List.mem p closed)) pipes with
      | [] -> ()
      | pipes -> aux pipes
  in aux

let all_nth_of_m l m n =
  let _, l' = List.fold_left
    (fun (count, accum) x ->
      (succ count) mod m,
      if count = n then
        x :: accum
      else
        accum)
    (0, [])
    l
  in List.rev l'

let map_async f ?(children = 4) l =
  let ret = ref [] in
  let handler_func x =
    ret := x :: (!ret)
  in
  let nth = all_nth_of_m l children in
  let child_funcs = List.map
    (fun n write ->
      List.iter
        (fun x -> write (f x))
        (nth n))
    (range children)
  in
  let pipes = List.map
    make_child
    child_funcs
  in child_loop handler_func (List.map snd pipes);
  !ret

let iter_async f ?(children = 4) l =
  let nth = all_nth_of_m l children in
  let child_funcs = List.map
    (fun n _ -> List.iter f (nth n))
    (range children)
  in
  let pipes = List.map
    make_child
    child_funcs
  in child_loop (fun () -> ()) (List.map snd pipes)
