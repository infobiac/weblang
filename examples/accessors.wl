f arr : a -> b
  a = [1,2,[3,4],5]
  c = a.[a.[1]].[1] // should be 4
  log c
