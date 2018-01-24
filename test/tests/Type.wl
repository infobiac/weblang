type A a : Num
  log "check a"
  a > 0

type B b : A
  log "check b"
  b > 1

type C c : B
  log "check c"
  c > 2

testType arg : Str -> Str
  x = 8
  if x :? C
    log "matches"
  else
    log "doesn't match"

  y = 1
  if y :? C
    log "matches"
  else
    log "doesn't match"
