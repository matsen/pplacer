(* Run embarrassingly parallel computation in multiple processes.

   This module implements an event loop and some classes that operate on top of
   the event loop in order to fork, compute things in the new processes, and
   transfer ocaml values between processes. Basic example classes are provided
   which implement map, iter, and fold.

   For more complicated processes, the classes provided can be extended.

*)

external quiet_close: int -> unit = "quiet_close"
external fd_of_file_descr: Unix.file_descr -> int = "%identity"

module OrderedFileDescr = struct
  type t = Unix.file_descr
  let compare a b = compare (fd_of_file_descr a) (fd_of_file_descr b)
end
module FDM = Map.Make(OrderedFileDescr)

type handler = {
  ch: in_channel;
  pid: int;
  handler: handler -> unit;
}

type 'a message =
  | Ready
  | Data of 'a
  | Exception of exn
  | Fatal_exception of exn

(* The main functionality for multiprocessing. Collects handlers from a group
 * of children, then dispatches incoming data from these children until every
 * incoming channel has been closed. *)
let event_loop children =
  Sys.set_signal Sys.sigchld Sys.Signal_ignore;
  let pipe_map = List.fold_left
    (fun m proc ->
      List.fold_left
        (fun m handler ->
          FDM.add
            (Unix.descr_of_in_channel handler.ch)
            handler
            m)
        m
        proc#handlers)
    FDM.empty
    children
  in

  let rec aux pipe_map =
    let pipes = FDM.fold
      (fun pipe _ l -> pipe :: l)
      pipe_map
      []
    in
    let rr, _, _ = Unix.select pipes [] [] 0.5 in
    let pipe_map' = List.fold_left
      (fun pipes p ->
        let handler = FDM.find p pipe_map in
        try
          handler.handler handler; pipes
        with
          | End_of_file -> FDM.remove p pipes)
      pipe_map
      rr
    in
    if FDM.is_empty pipe_map' then ()
    else aux pipe_map'
  in aux pipe_map

type buffer = {
  buf: string;
  mutable pos: int;
  length: int;
}
let buffer n = {buf = String.create n; pos = 0; length = n}
type buffer_state =
  | Needs_more
  | Done of string

let fill_buffer b ch =
  let ch = Unix.descr_of_in_channel ch in
  match Unix.read ch b.buf b.pos (b.length - b.pos) with
    | 0 -> raise End_of_file
    | n ->
      let n_read = b.pos + n in
      if n_read = b.length then
        Done b.buf
      else begin
        b.pos <- n_read;
        Needs_more
      end

let marshal ch x =
  Marshal.to_channel ch x [];
  flush ch

type marshal_recv_phase =
  | Needs_header of buffer
  | Needs_data of string * buffer

let rec range = function
  | 0 -> []
  | x -> (x - 1) :: range (x - 1)

(* Most of the important implementation bits for multiprocessing live in the
 * process class. It's responsible for doing the actual fork as well as handling
 * the IO. *)
class virtual ['a] process child_func =
  let child_rd, child_wr = Unix.pipe ()
  and parent_rd, parent_wr = Unix.pipe ()
  and progress_rd, progress_wr = Unix.pipe () in
  let _ = Unix.set_nonblock progress_rd in
  (* Only these descriptors are used in the child. The parent has no use for
   * them, so it closes them. The child has no use for anything but them, so
   * it closes everything but them. *)
  let child_only = [child_rd; parent_wr; progress_wr] in
  let () = flush_all () in
  let pid = match Unix.fork () with
    | 0 ->
      (* Do the actual closing of the irrelevant descriptors. *)
      begin
        let ignored = List.map fd_of_file_descr child_only in
        let ignored = match Ppatteries.memory_stats_ch with
          | None -> ignored
          | Some ch ->
            fd_of_file_descr (Unix.descr_of_out_channel ch) :: ignored
        in
        List.iter
          (fun fd -> if not (List.mem fd ignored) then quiet_close fd)
          (range 256)
      end;
      (* Make writing to stdout or stderr instead write to the progress
       * channel. *)
      Unix.dup2 progress_wr Unix.stdout;
      Unix.dup2 progress_wr Unix.stderr;
      Unix.close progress_wr;
      let rd = Unix.in_channel_of_descr child_rd
      and wr = Unix.out_channel_of_descr parent_wr in
      begin
        try
          child_func rd wr
        with
          | exn -> marshal wr (Fatal_exception exn)
      end;
      (* The child should only execute its function and not return control to
       * where the parent spawned it. *)
      exit 0
    | pid ->
      List.iter Unix.close child_only;
      pid
  in

object (self)
  val rd = Unix.in_channel_of_descr parent_rd
  val wr = Unix.out_channel_of_descr child_wr
  val progress = Unix.in_channel_of_descr progress_rd
  val pid = pid

  method rd = rd
  method wr = wr
  method progress = progress
  method pid = pid

  method handlers = [
    {pid; ch = rd; handler = self#marshal_recv};
    {pid; ch = progress; handler = self#progress_recv};
  ]

  method close =
    close_out wr

  (* Reading an incoming object has two parts:
   *  (a) reading the marshal header.
   *  (b) reading the marshal contents.
   * Once a whole cycle of this has been completed, call obj_received. *)
  method virtual obj_received: 'a message -> unit
  val mutable marshal_state = Needs_header (buffer 20)
  method private marshal_recv h =
    let b = match marshal_state with
      | Needs_header b -> b
      | Needs_data (_, b) -> b
    in match fill_buffer b h.ch, marshal_state with
      | Needs_more, _ -> ()
      | Done header, Needs_header _ ->
        marshal_state <- Needs_data (header, buffer (Marshal.data_size header 0))
      | Done body, Needs_data (header, _) ->
        let obj = Marshal.from_string (header ^ body) 0 in
        self#obj_received obj;
        marshal_state <- Needs_header (buffer 20)

  (* By setting the progress channel to work in nonblocking mode, we can use
   * ocaml's existing line buffering implementation instead of writing our
   * own. Reading an incomplete line raises Sys_blocked_io but otherwise keeps
   * the buffer intact. *)
  method virtual progress_received: string -> unit
  method private progress_recv h =
    match begin
      try
        Some (input_line h.ch)
      with
        | Sys_blocked_io -> None
    end with
      | None -> ()
      | Some "" -> raise End_of_file
      | Some line -> self#progress_received line
end

exception Child_error of exn
let default_progress_handler = Printf.printf "> %s\n"

(* The rest of this module is reference implementations of map and fold which
 * use the multiprocessing infrastructure. *)

class ['a, 'b] map_process ?(progress_handler = default_progress_handler)
  (f: 'a -> 'b) (q: 'a Queue.t) =

  (* In the case of map, the child sends a ready signal, then repeatedly reads
   * objects from its input channel until there are no more objects to read. The
   * read objects are applied to the supplied function, and then written back to
   * the parent. *)
  let child_func rd wr =
    marshal wr Ready;
    let rec aux () =
      match begin
        try
          Some (Marshal.from_channel rd)
        with
          | End_of_file -> None
      end with
        | Some x ->
          marshal
            wr
            begin
              try
                Data (f x)
              with
                | exn -> Exception exn
            end;
          aux ()
        | None -> close_in rd; close_out wr
    in aux ()
  in

object (self)
  inherit ['b] process child_func as super

  val q = q
  val mutable ret = []
  method ret = ret

  (* The parent halves of the children all share a Queue from which new objects
   * are taken in order to fairly distribute work. *)
  method push =
    match begin
      try
        Some (Queue.pop q)
      with
        | Queue.Empty -> None
    end with
      | Some x -> marshal wr x
      | None -> self#close

  method obj_received = function
    | Ready -> self#push
    | Data x -> ret <- x :: ret; self#push
    | Exception exn
    | Fatal_exception exn -> raise (Child_error exn)

  method progress_received = progress_handler
end

let queue_of_list l =
  let q = Queue.create () in
  List.iter (fun x -> Queue.push x q) l;
  q

let map ?(children = 4) ?progress_handler f l =
  let q = queue_of_list l in
  let children =
    List.map
      (fun _ -> new map_process ?progress_handler f q)
      (range children) in
  event_loop children;
  List.flatten (List.map (fun c -> c#ret) children)

let iter ?(children = 4) ?progress_handler f l =
  let q = queue_of_list l in
  let children =
    List.map
      (fun _ -> new map_process ?progress_handler f q)
      (range children) in
  event_loop children

class ['a, 'b] fold_process ?(progress_handler = default_progress_handler)
  (f: 'a -> 'b -> 'b) (q: 'a Queue.t) (initial: 'b) =

  (* fold is similar to map, except that there are no intermediate results. To
   * signal that it needs another object, it writes back None, and when the
   * parent closes the child's input channel, sends the final result. *)
  let child_func rd wr =
    marshal wr Ready;
    let rec aux prev =
      match begin
        try
          Some (Marshal.from_channel rd)
        with
          | End_of_file -> None
      end with
        | Some x ->
          marshal wr (Data None);
          aux (f x prev)
        | None -> Data (Some prev)
    in
    let res =
      try
        aux initial
      with
        | exn -> Exception exn
    in
    marshal wr res;
    close_in rd;
    close_out wr
  in

object (self)
  inherit ['b option] process child_func as super

  val q = q
  val mutable ret = initial
  method ret = ret

  method push =
    match begin
      try
        Some (Queue.pop q)
      with
        | Queue.Empty -> None
    end with
      | Some x -> marshal wr x
      | None -> self#close

  method obj_received = function
    | Ready -> self#push
    | Data Some x -> ret <- x; self#close
    | Data None -> self#push
    | Exception exn
    | Fatal_exception exn -> raise (Child_error exn)

  method progress_received = progress_handler
end

let fold ?(children = 4) ?progress_handler f l initial =
  let q = queue_of_list l in
  let children =
    List.map
      (fun _ -> new fold_process ?progress_handler f q initial)
      (range children)
  in
  event_loop children;
  List.map (fun c -> c#ret) children
