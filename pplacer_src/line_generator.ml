class line_generator ch =
  object
    val ch = ch

    method next =
      try input_line ch with | End_of_file -> raise Generator.Last
  end


