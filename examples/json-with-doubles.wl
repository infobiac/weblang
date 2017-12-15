
f a: intype -> outtype
  x = jn "{\"hi\":5, \"yo\":\"waddup\"}"
  log (get [x, "hi"])
