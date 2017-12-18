testConditional arg : Str -> Str
  x = true
  if x
    log "Inside if"
  else
    log "Inside else"

  log "Should be inside if above me"
