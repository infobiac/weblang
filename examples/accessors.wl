f arr : a -> b
  a = [1,2,[3,if 1 then 4 else 0],5]
  c = a.[a.[1]].[if 1 then 1 else 0] // should be 4
  log c
  d = {hi:"yo",red:"5",p:5}
  log d.["hi"]
  log d.["p"]
