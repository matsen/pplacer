let chop_revision s = Str.replace_first (Str.regexp "\\.[a-z]+[0-9]+") "" s

let version_revision = "v1.1.alpha02"
let version = chop_revision version_revision


