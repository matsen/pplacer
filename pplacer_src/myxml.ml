
let tag name ?(attributes = []) contents =
  Xml.Element (name, attributes, [Xml.PCData (contents)])

