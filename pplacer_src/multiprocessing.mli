exception Child_error of exn

type handler

type 'a message =
  | Ready
  | Data of 'a
  | Exception of exn
  | Fatal_exception of exn

class virtual ['a] process: (in_channel -> out_channel -> unit) ->
object
  method rd: in_channel
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
val range: int -> int list
