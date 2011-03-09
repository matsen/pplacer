val map_async: ('a -> 'b) -> ?children:int -> 'a list -> 'b list
val iter_async: ('a -> unit) -> ?children:int -> 'a list -> unit
val divide_async: ('a list -> 'b) -> ?children:int -> 'a list -> 'b list
