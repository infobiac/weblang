sendMsg arg : inType -> outType
  slackargs = jn "{\"url\":\"https://hooks.slack.com/services/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\"}"
  adds [slackargs, "payload", arg]
  log "done"
executeMsg arg: inType -> outType
  clientPost arg
  log "jordan es un buen nino"

