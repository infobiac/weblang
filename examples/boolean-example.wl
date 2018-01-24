bool t : a -> b
  a = 5==5
  b = 6==5+1
  c = 6==5
  d = 3+4==6
  e = 3+4==7
  f = 3>2
  g = 2>3
  h = 2<3
  i = 3<2
  j = 3>=3
  k = 3>=2
  l = 3>=4
  m = 3<=2
  n = 2<=2
  o = 2<=3

  log a
  log b
  log c
  log d
  log e
  log f
  log g
  log h
  log i
  log k
  log l
  log m
  log n
  log o
  
  if 5==5
    log "worked"
  else
    log "nope"
  if 5>6
    log "nope"
  else
    log "worked"
