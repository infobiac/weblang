sendMsg arg : Str -> Obj
  args = {}
  body = {}
  body = addToObj [body, "'message'", "'wadup jord'"]
  args = addToObj [args, "payload", body]
  args = addToObj [args, "url", "https://messagingapi.sinch.com/v1/sms/+17075707115"]
  log args
  test = clientPost args
  log test
