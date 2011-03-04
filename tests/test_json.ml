open OUnit
open Test_util

let suite = List.map
  (fun fname ->
    let name = Filename.basename fname in
    name >:: match String.sub name 0 4 with
      | "pass" -> fun () ->
        let parsed = Json.of_file fname in
        let roundtrip = Json.of_string (Json.to_string parsed) in
        "not equal after roundtrip" @? json_equal parsed roundtrip
      | "fail" -> fun () ->
        "parsing didn't fail" @? begin
          try
            let _ = Json.of_file fname in false
          with
            | Jsontype.Parse_error _ -> true
        end
      | _ -> failwith (Printf.sprintf "unexpected json file %s" fname)
  )
  (Common_base.get_dir_contents
     ~pred:(fun name -> Filename.check_suffix name "json")
     (tests_dir ^ "json"))
