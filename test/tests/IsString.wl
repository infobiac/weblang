testIsString arg : Str -> Str
  a = "yes"
  b = 9
  c = ["yes again",9]
  check1 = isString a
  check2 = isString b
  check3 = isString c
  check4 = isString c.[0]
  check5 = isString c.[1]
  log check1
  log check2
  log check3
  log check4
  log check5
