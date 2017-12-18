testJsonDoubles arg : Str -> Str
  x = jn "{\"one\": 69, \"two\":\"get your mind out of the gutter\"}"
  log (get [x, "one"])
  log (get [x, "two"])
