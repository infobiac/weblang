testIsNum arg: Str -> Str
  a = 5
  b = "hi"
  c = jn "{\"one\": 6}"
  d = (get [c, "one"])
  check1 = isNum a
  check2 = isNum b
  check3 = isNum c
  check4 = isNum d
  log check1
  log check2
  log check3
  log check4
