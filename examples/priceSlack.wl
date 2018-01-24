import {url: "https://hooks.slack.com/services/T74RW7J0N/B891X5YNN/", key:"", secret:"",
	endpoints:[{fnName:"sendSlackMsg", endpoint:"BaQHlflLTmQQNKHH3EE6PrR1", is_post:true}] }

import {url: "https://api.gdax.com/products/", key:"", secret:"",
	endpoints:[{fnName:"getBitcoinPrice", endpoint:"btc-usd/ticker", is_post:false},
{fnName:"getEtherPrice", endpoint:"eth-usd/ticker", is_post:false},
{fnName:"getLitecoinPrice", endpoint:"ltc-usd/ticker", is_post:false}] }

getAllPrices arg : Str -> Str
  bitcoin arg
  ether arg
  litecoin arg

bitcoin arg : Str -> Str
  x = getBitcoinPrice arg
  res = jn x
  precio = (get [res, "price"])
  sendMsg "Latest Price of Bitcoin in USD: "
  sendMsg precio

ether arg : Str -> Str
  x = getEtherPrice arg
  res = jn x
  precio = (get [res, "price"])
  sendMsg "Latest Price of Ethereum in USD: "
  sendMsg precio

litecoin arg : Str -> Str
  x = getLitecoinPrice arg
  res = jn x
  precio = (get [res, "price"])
  sendMsg "Latest Price of Litecoin in USD: "
  sendMsg precio

sendMsg arg : Str -> Obj
  empty = jn "{}"
  channel = addToObj [empty, "channel", "#crypto"]
  user = addToObj [channel, "username", "CryptoEdwards"]
  emoji = addToObj [user, "icon_emoji",":edwards:"]
  text = addToObj [emoji, "text",arg]
  sendSlackMsg text
