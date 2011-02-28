(* an object for named sequences *)

class sr ~(name:string) ~(seq:string) =
  object
    val name = name
    val seq = seq
    method get_name = name
    method get_seq = seq
  end;;



