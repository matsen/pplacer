(* pplacer v1.0. Copyright (C) 2009-2010  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *
 * Makes a type for edges, with cached glv calculations.
 * Evolv is orig evolved along an edge of length bl.
 *
 * Scary side-effect land! But by keeping with the interface, we should be safe.
 *)

type glv_edge = {
  orig   : Glv.glv ref;
  evolv  : Glv.glv;
  bl     : float ref;
}

let get_bl e = !(e.bl)

let get_evolv e = 
  e.evolv

let recalculate model glve = 
  Glv.evolve_into model 
                  ~dst:(glve.evolv) ~src:(!(glve.orig)) !(glve.bl)

let set_bl model glve new_bl = 
  glve.bl := new_bl;
  recalculate model glve

let make model orig init_bl = 
  let glve = 
    { orig  = ref orig;
      evolv = Glv.copy orig;
      bl    = ref init_bl } in
  recalculate model glve;
  glve

let set_orig_and_bl model glve new_orig new_bl = 
  glve.orig := new_orig;
  glve.bl := new_bl;
  recalculate model glve

