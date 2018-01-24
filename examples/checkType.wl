type A a : Num
  log "check a"
  a > 0

type B b : A
  log "check b"
  b > 1

type C c : B
  log "check c"
  c > 2

f x : Num -> Str
  if x :? C
    log "matches"
  else
    log "doesn't match"
