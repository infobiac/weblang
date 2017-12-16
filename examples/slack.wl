sendMsg arg : Str -> Obj
  js = jn "{\"url\":\"https://hooks.slack.com/services/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\" }"
  args = addToObj [js, "payload", arg]
  executeMsg args

executeMsg arg: Obj -> Str
  clientPost arg
  log "jordan es un buen nino"

