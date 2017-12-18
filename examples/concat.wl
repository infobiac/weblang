
f arg : Str -> Str
  full = "this"
  b = " is "
  c = "a"
  d = " sentence."
  full = cat [full, b]
  full = cat [full, c]
  full = cat [full, d]
  log full
