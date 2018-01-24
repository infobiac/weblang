testIsArr arg : Str -> Bool
  a = ["one",5]
  b = 7
  check1 = isArr a
  check2 = isArr b
  check3 = isArr a.[1]
  log check1
  log check2
  log check3
