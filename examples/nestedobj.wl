


f arg : Str -> Str
  //use: "{\"test\":{\"one\":\"two\"},\"arr\":[1,2,3]}"
  ajn = jn arg
  log "parsed"
  log ajn
  log ajn.["test"]
  log ajn.["arr"]
