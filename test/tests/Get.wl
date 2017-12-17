testGet arg : Str -> Str
  endpoint = jn "{\"url\":\"api.gdax.com/products/eth-usd/ticker\", \"body\":\"\" }"
  res = clientGet endpoint
  check = isObj res
  log check
