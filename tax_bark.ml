(* pplacer v1.0. Copyright (C) 2009-2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer.  If not, see <http://www.gnu.org/licenses/>.
 *
 * bark for trees that have newick + a taxonomic annotation
*)

open Fam_batteries
open MapsSets


class tax_bark arg = 
  let (bl, name, boot, tax_ido) = 
    match arg with
    | `Empty -> (None, None, None, None)
    | `Of_newick_bark (nb, tax_ido) -> 
        (nb#get_bl_opt, nb#get_name_opt, nb#get_boot_opt, tax_ido)
  in
  object (* (self) *)
    val tax_ido = tax_ido
    inherit Newick_bark.newick_bark 
      (`Of_bl_name_boot (bl, name, boot))
      as super

    method get_tax_ido = tax_ido
    method set_tax_ido tio = {< tax_ido = tio >}
    method set_tax_id ti = {< tax_ido = Some ti >}

    method ppr ff = 
      Format.fprintf ff "@[{%a taxid = %a;}@]" 
        (fun ff () -> super#ppr_inners ff) ()
        (Ppr.ppr_opt Tax_id.ppr) tax_ido

    method write_xml ch = 
      super#write_xml ch;
      (* sort so tags are in proper order *)
      match tax_ido with
      | Some x -> 
          Xml.write_long_tag
            (fun () -> Tax_id.write_xml ch x)
            "taxonomy"
            ch
      | None -> ()
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

let of_newick_bark nb tax_ido = 
  new tax_bark (`Of_newick_bark(nb, tax_ido))
