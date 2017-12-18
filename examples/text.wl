sendMsg arg : Str -> Obj
  args = {}
  body = {}
  body = addToObj [body, "'message'", "'wadup jord'"]
  args = addToObj [args, "payload", body]
  args = addToObj [args, "key", "73e6c7dd-776b-4ec2-9b4b-965e1a1dcebe"]
  args = addToObj [args, "secret", "sCatHtlfB0yih7l/lhGpAA=="]
  args = addToObj [args, "url", "https://messagingapi.sinch.com/v1/sms/+17075707115"]
  log args
  test = clientPost args
  log test
