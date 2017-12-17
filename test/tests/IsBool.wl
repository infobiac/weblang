testIsBool arg : Str -> Str
  a = 5
  b = true
  c = "true"
  d = isNum a
  check1 = isBool a
  check2 = isBool b
  check3 = isBool c
  check4 = isBool d
  log check1
  log check2
  log check3
  log check4
