testIsObj arg : Str -> Str
  a = 5
  b = "hi"
  c = ["hi", 5]
  d = isString c.[0]
  e = jn "{\"hi\":\"3\"}"
  f = jn "{\"test\":{\"one\":\"two\"},\"arr\":[1,2,3],\"num\":7}"
  check1 = isObj a
  check2 = isObj b
  check3 = isObj c
  check4 = isObj d
  check5 = isObj e
  check6 = isObj f
  check7 = isObj f.["test"]
  check8 = isObj f.["arr"]
  check9 = isObj f.["num"]
  log check1
  log check2
  log check3
  log check4
  log check5
  log check6
  log check7
  log check8
  log check9
