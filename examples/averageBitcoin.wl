import {url: "https://hooks.slack.com/services/T74RW7J0N/B891X5YNN/", key:"", secret:"",
	endpoints:[{fnName:"sendSlackMsg", endpoint:"BaQHlflLTmQQNKHH3EE6PrR1", is_post:true}] }

import {url: "https://cex.io/api",key:"", secret:"",
	endpoints:[{fnName:"cexBitcoinPrice", endpoint:"ticker/BTC/USD", is_post:false}] }

import {url: "https://www.bitstamp.net/api", key:"", secret:"",
	endpoints:[{fnName:"bitstampBitcoinPrice", endpoint:"ticker/", is_post:false}] }

import {url: "https://api.bitfinex.com/v2/", key:"", secret:"",
	endpoints:[{fnName:"bitfinexBitcoinPrice", endpoint:"ticker/tBTCUSD", is_post:false}] }

import {url: "https://api.gemini.com/v1", key:"", secret:"",
	endpoints:[{fnName:"geminiBitcoinPrice", endpoint:"pubticker/BTCUSD", is_post:false}] }

import {url: "https://api.gdax.com/products/", key:"", secret:"",
	endpoints:[{fnName:"getBitcoinPrice", endpoint:"btc-usd/ticker", is_post:false}]}

getAvgPrice arg : Str -> Str
  arr = []
  gdaxprice = gdax arg
  arr = push [arr, gdaxprice]
  cexprice = cex arg
  arr = push [arr,cexprice]
  bitfinexprice = bitfinex arg
  arr = push [arr,bitfinexprice]
  bitstampprice = bitstamp arg
  arr = push [arr, bitstampprice]  
  geminiprice = gemini arg
  arr = push [arr, geminiprice]
  log arr
  average = avg arr
  log average
  sendMsg "Average Bitcoin price between GDAX, CEX, BITFINEX, GEMINI, and BITSTAMP:"
  x = cat ["",average]
  sendMsg x

gdax arg : Str -> Str
  x = getBitcoinPrice arg
  res = jn x
  precio = (get [res, "price"])
  precio

cex arg : Str -> Str
  x = cexBitcoinPrice arg
  res = jn x
  precio = get [res, "ask"]
  precio

bitfinex arg : Str -> Str
  x = bitfinexBitcoinPrice arg
  res = jn x
  precio = res.[0]
  precio

gemini arg : Str -> Str
  x = geminiBitcoinPrice arg
  res = jn x
  precio = get [res, "ask"]
  precio

bitstamp arg : Str -> Str
  x = bitstampBitcoinPrice arg
  res = jn x
  precio = get [res, "ask"]
  precio

sendMsg arg : Str -> Obj
  empty = jn "{}"
  channel = addToObj [empty, "channel", "#crypto"]
  user = addToObj [channel, "username", "CryptoEdwards"]
  emoji = addToObj [user, "icon_emoji",":edwards:"]
  text = addToObj [emoji, "text",arg]
  log text
  x = sendSlackMsg text

/* Get the average of all numbers in an array.
   Input an array of numbers. */

avg arg : Arr -> Num
  count = 0
  total = 0
  foreach x in arg
    total = total + arg.[count]
    count = count + 1
  result = (total/count)
