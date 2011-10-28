type tag = {
  name: string;
  attrs: (string * string) list;
  contents: string;
  children: tag list;
}

let tag name ?(attrs = []) ?(children = []) contents =
  {name; attrs; contents; children}
