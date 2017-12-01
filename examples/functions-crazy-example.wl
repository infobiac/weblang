main arg : inType -> outType
  log "Hello world"
  otherFunction "random"

otherFunction arg : someType -> someOtherType
  log "logging this message"
  someOtherFunction "whatever"

someOtherFunction arg : someType -> someOtherType
  log "logging other message"
