include "examples/stdlib.wl"

import {url: "https://hooks.slack.com/services/T74RW7J0N/B891X5YNN/", key:"", secret:"", header:"", endpoints:[{fnName:"sendSlackMsg", endpoint:"BaQHlflLTmQQNKHH3EE6PrR1", is_post:true}] }

import {url: "https://cex.io/api",key:"", secret:"", header:"",
	endpoints:[{fnName:"cexBitcoinPrice", endpoint:"ticker/BTC/USD", is_post:false}] }

import {url: "https://www.bitstamp.net/api", key:"", secret:"", header:"",
	endpoints:[{fnName:"bitstampBitcoinPrice", endpoint:"ticker/", is_post:false}] }

import {url: "https://api.bitfinex.com/v2/", key:"", secret:"", header:"",
	endpoints:[{fnName:"bitfinexBitcoinPrice", endpoint:"ticker/tBTCUSD", is_post:false}] }

import {url: "https://api.gemini.com/v1", key:"", secret:"", header:"",
	endpoints:[{fnName:"geminiBitcoinPrice", endpoint:"pubticker/BTCUSD", is_post:false}] }

import {url: "https://api.gdax.com/products/", key:"", secret:"", header:"", 
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
  average = avg arr
  sendMsg "Average Bitcoin price between GDAX, CEX, BITFINEX, GEMINI, and BITSTAMP:"
  x = cat ["",average]
  sendMsg x

gdax arg : Str -> Num
  x = getBitcoinPrice arg
  res = jn x
  precio = (get [res, "price"])
  if isString precio
    precio = toNum precio
  else
    0
  precio

cex arg : Str -> Num
  x = cexBitcoinPrice arg
  res = jn x
  precio = get [res, "ask"]
  if isString precio
    precio = toNum precio
  else
    0
  precio

bitfinex arg : Str -> Num
  x = bitfinexBitcoinPrice arg
  res = jn x
  precio = res.[0]
  if isString precio
    precio = toNum precio
  else
    0
  precio

gemini arg : Str -> Num
  x = geminiBitcoinPrice arg
  res = jn x
  precio = get [res, "ask"]
  if isString precio
    precio = toNum precio
  else
    0
  precio

bitstamp arg : Str -> Num
  x = bitstampBitcoinPrice arg
  res = jn x
  precio = get [res, "ask"]
  if isString precio
    precio = toNum precio
  else
    0
  precio

sendMsg arg : Str -> Obj
  empty = jn "{}"
  channel = addToObj [empty, "channel", "#crypto"]
  user = addToObj [channel, "username", "CryptoEdwards"]
  emoji = addToObj [user, "icon_emoji",":edwards:"]
  text = addToObj [emoji, "text",arg]
  log text
  x = sendSlackMsg text

