sendMsg arg : inType -> outType
  js = jn "{\"url\":\"https://hooks.slack.com/services/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\" }"
  args = addToObj [js, "payload", arg]
  executeMsg args

executeMsg arg: inType -> outType
  clientPost arg
  log "jordan es un buen nino"

