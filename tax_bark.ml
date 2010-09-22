(* pplacer v1.0. Copyright (C) 2009-2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer.  If not, see <http://www.gnu.org/licenses/>.
 *
 * bark for trees that have newick + a taxonomic annotation
*)

open Fam_batteries
open MapsSets

let write_xml_tax_name ch = Xml.write_tag output_string "scientific_name" ch

class tax_bark arg = 
  let (bl, name, boot, tax_ido, tax_nameo) = 
    match arg with
    | `Empty -> (None, None, None, None, None)
    | `Of_newick_bark (nb, tax_ido, tax_nameo) -> 
        (nb#get_bl_opt, nb#get_name_opt, nb#get_boot_opt, tax_ido, tax_nameo)
  in
  object (* (self) *)
    val tax_ido = tax_ido
    val tax_nameo = tax_nameo
    inherit Newick_bark.newick_bark 
      (`Of_bl_name_boot (bl, name, boot))
      as super

    method get_tax_ido = tax_ido
    method set_tax_ido tio = {< tax_ido = tio >}
    method set_tax_id ti = {< tax_ido = Some ti >}

    method get_tax_nameo = tax_nameo
    method set_tax_nameo tno = {< tax_nameo = tno >}
    method set_tax_name tn = {< tax_nameo = Some tn >}

    method ppr ff = 
      Format.fprintf ff "@[{%a taxid = %a;}@]" 
        (fun ff () -> super#ppr_inners ff) ()
        (Ppr.ppr_opt Tax_id.ppr) tax_ido

    method write_xml ch = 
      let perhaps_write write_fun xo = 
        match xo with 
        | Some x -> write_fun x 
        | None -> ()
      in
      super#write_xml ch;
      (* sort so tags are in proper order *)
      match (tax_ido, tax_nameo) with
      | (None, None) -> ()
      | _ -> 
          Xml.write_long_tag
            (fun () ->
              perhaps_write (Tax_id.write_xml ch) tax_ido;
              perhaps_write (write_xml_tax_name ch) tax_nameo;)
            "taxonomy"
            ch
  end

  (*
let compare b1 b2 = 
  try 
    Base.raise_if_different Newick_bark.compare b1 b2;
    Base.raise_if_different compare b1#get_decor b2#get_decor;
    0
  with
  | Base.Different c -> c
*)

let of_newick_bark nb tax_ido tax_nameo = 
  new tax_bark (`Of_newick_bark(nb, tax_ido, tax_nameo))
