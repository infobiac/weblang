
a arg : inType -> outType
  testjson = jn "{\"test\":\"hey\"}"
  result = get [testjson,"test"]
  log "test"
  log result
  added = addToObj [testjson, "hi", "yo"]
  test = get [added,"hi"]
  log added
