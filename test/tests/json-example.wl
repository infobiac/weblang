

type Integral x : Number[integral]

type Natural x : Integral
  x > 0

x = ["first", 2, [3.0], {four: 4}]

a arg : inType -> outType
  testjson = jn "{\"test\":\"hey\"}"
  result = gets[testjson,"test"]
  log "test"
  log result
