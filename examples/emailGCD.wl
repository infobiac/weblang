include "examples/stdlib.wl"

sendMsg arg : Str -> Obj
  log arg
  x = isObj arg
  log x
  jargs = jn arg
  x = isObj arg
  log x
  phone = get [jargs, "phone"]
  log phone
  nums = get [jargs, "gcd"]
  log nums
  nums = cat ["[", nums]
  nums = cat [nums,"]"]
  nums = jn nums
  log nums
  log (isArr nums)
  greatest = gcd nums
  x = nums.[0]
  y = nums.[1]
  msg = cat ["The greatest common denominator between ", x]
  msg = cat [msg, " and "]
  msg = cat [msg, y]
  msg = cat [msg, " is "]
  msg = cat [msg, greatest]  
  args = {}
  body = {}
  body = addToObj [body, "message", msg]
  args = addToObj [args, "payload", body]
  args = addToObj [args, "key", "73e6c7dd-776b-4ec2-9b4b-965e1a1dcebe"]
  args = addToObj [args, "secret", "sCatHtlfB0yih7l/lhGpAA=="]
  u = cat ["https://messagingapi.sinch.com/v1/sms/",phone]
  args = addToObj [args, "url", u]
  log args
  test = clientPost args
  log test

