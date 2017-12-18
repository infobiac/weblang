import {url: "https://api.gdax.com/products/",
	endpoints:[{fnName:"getEtherPrice", endpoint:"eth-usd/ticker", is_post:false}] }


testGet arg : Str -> Str
  response = getEtherPrice arg
  res = jn response
  check = isObj res
  log check
