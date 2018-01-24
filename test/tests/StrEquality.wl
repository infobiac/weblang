testStrEquality arg : Str -> Str
  x = "hello"
  y = "hola"
  z = "hello"
  w = "hello "
  log (equals [x,"hello"])
  log (equals [x,y])
  log (equals [x,z])
  log (equals [x,w])
  "bye"
