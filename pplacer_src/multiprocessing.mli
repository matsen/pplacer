val map:
  ?children:int -> ?progress_handler:(string -> unit) ->
  ('a -> 'b) -> 'a list -> 'b list

val iter:
  ?children:int -> ?progress_handler:(string -> unit) ->
  ('a -> unit) -> 'a list -> unit

val fold:
  ?children:int -> ?progress_handler:(string -> unit) ->
  ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b list
