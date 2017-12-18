import {url: "https://hooks.slack.com/services/T74RW7J0N/B891X5YNN/", key:"", secret:"",
	endpoints:[{fnName:"sendSlackMsg", endpoint:"BaQHlflLTmQQNKHH3EE6PrR1", is_post:true}] }

import {url: "https://api.gdax.com/products/", key:"", secret:"",
	endpoints:[{fnName:"getBitcoinPrice", endpoint:"btc-usd/ticker", is_post:false},
{fnName:"getEtherPrice", endpoint:"eth-usd/ticker", is_post:false},
{fnName:"getLitecoinPrice", endpoint:"ltc-usd/ticker", is_post:false}] }

getAllPrices arg : Str -> Str
  b= bitcoin arg
  e = ether arg
  l =litecoin arg
  a = jn "{}"
  a = addToObj [a,"bitcoin",b]
  a = addToObj [a,"ethereum",e]
  a = addToObj [a,"litecoin",l]
  log a

bitcoin arg : Str -> Str
  x = getBitcoinPrice arg
  res = jn x
  precio = (get [res, "price"])
  precio

ether arg : Str -> Str
  x = getEtherPrice arg
  res = jn x
  precio = (get [res, "price"])
  precio

litecoin arg : Str -> Str
  x = getLitecoinPrice arg
  res = jn x
  precio = (get [res, "price"])
  precio

