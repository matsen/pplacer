let base_version = "v1.1.alpha09"
let version = match Git_version.version with
  | Some git_version -> Printf.sprintf "%s (git %s)" base_version git_version
  | None -> base_version
