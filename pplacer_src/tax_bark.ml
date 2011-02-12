(* pplacer v1.0. Copyright (C) 2009-2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer.  If not, see <http://www.gnu.org/licenses/>.
 *
 * bark for trees that have newick + a taxonomic annotation
*)

open Fam_batteries
open MapsSets

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

    method to_xml =
      super#to_xml @
        let maybe_list f = function
          | Some x -> f x
          | None -> []
        in match tax_ido, tax_nameo with
          | None, None -> []
          | _ -> Myxml.tag "taxonomy" (
            maybe_list Tax_id.to_xml tax_ido
            @ maybe_list (fun name -> [Myxml.tag "scientific_name"]) tax_nameo
          )
  end

let of_newick_bark nb tax_ido tax_nameo =
  new tax_bark (`Of_newick_bark(nb, tax_ido, tax_nameo))
