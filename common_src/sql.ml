open Sqlite3
module D = Sqlite3.Data

let check ?(allowed = [Rc.OK]) db retcode =
  if not (List.mem retcode allowed) then
    raise (Error (errmsg db))

let check_exec db ?cb ?allowed s =
  check ?allowed db (exec ?cb db s)

let bind_arr ?allowed db st arr =
  Array.iteri
    (fun e x -> check ?allowed db (bind st (e + 1) x))
    arr

let bind_step_reset db st params =
  bind_arr db st params;
  check ~allowed:[Rc.DONE] db (step st);
  check db (reset st)

let close db =
  let _ = db_close db in ()

