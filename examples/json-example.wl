

type Integral x : Number[integral]

type Natural x : Integral
  x > 0

x = ["first", 2, [3.0], {four: 4}]

main arg : inType -> outType
  js ["{test:hey}"]
  jgets [j, "test"]
  log "hey"
