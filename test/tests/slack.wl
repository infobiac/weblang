sendMsg arg : inType -> outType
  slackargs = "{\"url\":\"https://hooks.slack.com/services/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\", \"payload\": \"{'text':'Hey Lizzie, how are you? Check out this emoji :edwards:'}\"}"
  executeMsg slackargs

executeMsg arg: inType -> outType
  clientPost arg
  log "jordan es un buen nino"

