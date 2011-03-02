open OUnit
open Test_util

let suite_of_prl name =
  Printf.sprintf "roundtrip_%s" name >::: List.map
    (fun pr ->
      (pr.Placerun.name) >:: fun () ->
        "not equal to self" @? placerun_equal pr pr;
        let fname = Filename.temp_file "pr" ".json" in
        let finish () = Unix.unlink fname in begin
          try
            Placerun_io.to_json_file "" fname pr;
            let roundtripped = Placerun_io.of_json_file fname in
            "not equal to roundtrip" @? placerun_equal pr roundtripped
          with
            | exn -> finish (); raise exn;
        end; finish ())
    (placeruns_of_dir name)

let suite = [
  suite_of_prl "simple";
  suite_of_prl "psbA";
  suite_of_prl "moran";
]
