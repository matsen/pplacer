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
 
class newick_bark = 
  object (self)
    val bl = None
    val name = None
    val boot = None

    method get_bl = 
      match bl with
      | Some x -> x
      | None -> raise No_bl

    method set_bl (x:float) = {< bl = Some x >}

    method get_name = 
      match name with
      | Some s -> s
      | None -> raise No_name

    method set_name s = {< name = Some s >}

    method get_boot =
      match boot with
      | Some x -> x
      | None -> raise No_boot
      
    method set_boot x = {< boot = Some x >}

    method to_newick_string = 
      (opt_val_to_string gstring_of_float boot) ^ 
      (opt_val_to_string (fun s -> s) name) ^ 
      (opt_val_to_string (fun x -> ":"^(gstring_of_float x)) bl)

    method private ppr_inners ff = 
      ppr_opt_named "bl" Format.pp_print_float ff bl;
      ppr_opt_named "name" Format.pp_print_string ff name;
      ppr_opt_named "boot" Format.pp_print_float ff boot

    method ppr ff = 
      Format.fprintf ff "{%a}" (fun ff () -> self#ppr_inners ff) ()

  end

let map_find_loose id m = 
  if Bark_map.mem id m then Bark_map.find id m
  else new newick_bark

let map_set_bl id bl m = 
  Bark_map.add id ((map_find_loose id m)#set_bl bl) m

let map_set_name id name m = 
  Bark_map.add id ((map_find_loose id m)#set_name name) m

let map_set_boot id boot m = 
  Bark_map.add id ((map_find_loose id m)#set_boot boot) m
