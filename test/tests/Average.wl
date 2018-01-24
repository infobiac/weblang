include "examples/stdlib.wl"

testAverage arg : Str -> Str
  x = [8,2]
  y = [10,20,30]
  log (avg x)
  z = avg y
  log z
  "bye"
