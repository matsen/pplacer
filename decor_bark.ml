exception No_bl
exception No_name
exception No_boot

let gstring_of_float x = Printf.sprintf "%g" x

let opt_val_to_string val_to_string = function
  | Some x -> val_to_string x
  | None -> ""

class decor_bark = 
  object
    inherit Newick_bark.newick_bark  

    val decor = ref []

    (* after integrating, change this to Decor.decor *)
    method get_decor = (!decor : Decor.decoration list)
    method set_decor d = decor := d
  end

let b = new decor_bark


