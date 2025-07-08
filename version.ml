let version = 
  try
    let ic = Unix.open_process_in "git describe --tags --always --dirty 2>/dev/null" in
    let version = input_line ic in
    close_in ic;
    version
  with _ -> "dev"
