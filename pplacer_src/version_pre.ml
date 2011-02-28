let chop_revision s = Str.replace_first (Str.regexp "\\.r[0-9]+") "" s

let version_revision = "VERSION"
let version = chop_revision version_revision


