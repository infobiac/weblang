testAllTypes arg : Str -> Str
  a = 5
  b = "hi"
  c = ["hi", 5]
  d = isString a
  e = isString b
  f = isNum a
  g = isNum b
  h = isArr c
  i = isArr (c.[0])
  j = isString c.[0]
  k = jn "{\"hi\":\"3\"}"
  l = isString k
  m = isNum k
  n = isObj k
  o = isObj a
  p = isObj b
  q = true
  r = isObj q
  s = isBool q
  t = isNum q
  log d
  log e
  log f
  log g
  log h
  log i
  log j
  log k
  log l
  log m
  log n
  log o
  log p
  log q
  log r
  log s
  log t
