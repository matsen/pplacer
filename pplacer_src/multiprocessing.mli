exception Child_error of exn

type handler

type 'a message =
  | Ready
  | Data of 'a
  | Exception of exn
  | Fatal_exception of exn

(** Process class for multiprocessing.
    The child function receives an in_channel for reading and a Unix.file_descr
    for writing. Raw file descriptors are used for writing to avoid OCaml 5.x
    channel buffering deadlocks that occur when using out_channels after fork. *)
class virtual ['a] process: (in_channel -> Unix.file_descr -> unit) ->
object
  method rd: in_channel
  (* Raw file descriptor for writing. Preferred for OCaml 5.x compatibility. *)
  method wr_fd: Unix.file_descr
  (* Out channel for writing. Deprecated: use wr_fd with marshal_to_fd instead
     to avoid potential buffering issues in OCaml 5.x. *)
  method wr: out_channel
  method progress: in_channel
  method pid: int
  method handlers: handler list
  method close: unit
  method virtual obj_received: 'a message -> unit
  method virtual progress_received: string -> unit
end

val map:
  ?children:int -> ?progress_handler:(string -> unit) ->
  ('a -> 'b) -> 'a list -> 'b list

val iter:
  ?children:int -> ?progress_handler:(string -> unit) ->
  ('a -> unit) -> 'a list -> unit

val fold:
  ?children:int -> ?progress_handler:(string -> unit) ->
  ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b list

val event_loop: 'a process list -> unit
val queue_of_list: 'a list -> 'a Queue.t
val marshal: out_channel -> 'a -> unit

(** Write marshaled data directly to a file descriptor. Preferred for OCaml 5.x. *)
val marshal_to_fd: Unix.file_descr -> 'a -> unit

(** Write all bytes to a file descriptor, handling partial writes. *)
val write_all_bytes: Unix.file_descr -> bytes -> unit

(** Close child process channels: the read channel and write file descriptor. *)
val close_child_channels: in_channel -> Unix.file_descr -> unit

val try_fork: int -> ('a -> 'b) -> 'a -> 'b list
