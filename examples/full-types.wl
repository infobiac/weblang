type Pos x : Num
  zero = 0
  x > zero

type Odd s : Pos
  (s - 1) % 2 == 0

f x : Str -> Pos
  y = 3
  log (incOdd y)

incOdd x : Odd -> Pos{val % 2 == 0}
  x + 1
