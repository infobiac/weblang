sendMsg arg : inType -> outType
  slackargs = "{\"url\":\"https://hooks.slack.com/services/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\", \"payload\": \"{'text':'We have the best merch in the game! <http://clima.life|Click here> for details!\nSent by webLang engineering'}\"}"
  executeMsg slackargs

executeMsg arg: inType -> outType
  clientPost arg
  log "jordan es un buen nino"
