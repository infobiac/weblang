testConditionalElse arg : Str -> Str
  x = false
  if x
    log "Inside if"
  else
    log "Inside else"

  log "Should be inside else above me"
