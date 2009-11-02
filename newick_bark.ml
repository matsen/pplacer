exception No_bl
exception No_name
exception No_boot

let gstring_of_float x = Printf.sprintf "%g" x

let opt_val_to_string val_to_string = function
  | Some x -> val_to_string x
  | None -> ""

let ppr_opt_named name ppr_val ff = function
  | Some x -> Format.fprintf ff " %s = %a;@," name ppr_val x
  | None -> ()

class type newick_bark_type = 
  object
    method get_bl : float
    method set_bl : float -> unit
    method get_name : string
    method set_name : string -> unit
    method get_boot : float
    method set_boot : float -> unit
    method ppr : Format.formatter -> unit
  end
 
class newick_bark = 
  object (self)
    val bl = ref None
    val name = ref None
    val boot = ref None

    method get_bl = 
      match !bl with
      | Some x -> x
      | None -> raise No_bl

    method set_bl (x:float) = 
      bl := Some x

    method get_name = 
      match !name with
      | Some s -> s
      | None -> raise No_name

    method set_name s = 
      name := Some s

    method get_boot =
      match !boot with
      | Some x -> x
      | None -> raise No_boot
      
    method set_boot x =
      boot := Some x

    method to_newick_string = 
      (opt_val_to_string gstring_of_float !boot) ^ 
      (opt_val_to_string (fun s -> s) !name) ^ 
      (opt_val_to_string (fun x -> ":"^(gstring_of_float x)) !bl)

    method ppr_inners ff = 
      ppr_opt_named "bl" Format.pp_print_float ff !bl;
      ppr_opt_named "name" Format.pp_print_string ff !name;
      ppr_opt_named "boot" Format.pp_print_float ff !boot

    method ppr ff = 
      Format.fprintf ff "{%a}" (fun ff () -> self#ppr_inners ff) ()

  end

let b = new newick_bark

let ppr_newick_bark ff nb = 
  Format.fprintf ff "%s" (nb#to_newick_string)


