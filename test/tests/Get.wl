import {url: "https://api.gdax.com/products/", key : "", secret: "", header:"",
	endpoints:[{fnName:"getEtherPrice", endpoint:"eth-usd/ticker", is_post:false}] }


testGet arg : Str -> Str
  response = getEtherPrice arg
  res = jn response
  check = isObj res
  log check
