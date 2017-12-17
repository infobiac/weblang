testJsonAdd arg : Str -> Str
  testjson = jn "{\"test\":\"Json get works\"}"
  result = get [testjson,"test"]
  log result
  added = addToObj [testjson, "test2", "Json add works"]
  test = get [added,"test2"]
  log test
