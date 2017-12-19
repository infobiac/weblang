include "examples/stdlib.wl"

import {url: "https://hooks.slack.com/services/T74RW7J0N/B891X5YNN/", key: "", secret:"", header:"",
	endpoints:[{fnName:"sendSlackMsg", endpoint:"BaQHlflLTmQQNKHH3EE6PrR1", is_post:true}] }

import {url: "https://messagingapi.sinch.com/v1/sms/", key:"73e6c7dd-776b-4ec2-9b4b-965e1a1dcebe", secret:"sCatHtlfB0yih7l/lhGpAA==", header:"",endpoints:[{fnName:"sendJordanTxt", endpoint:"+17075707115", is_post:true}]}

import {url: "https://api.gdax.com/products/", key:"", secret:"", header:"",
	endpoints:[{fnName:"getBitcoinPrice", endpoint:"btc-usd/ticker", is_post:false},
{fnName:"getEtherPrice", endpoint:"eth-usd/ticker", is_post:false},
{fnName:"getLitecoinPrice", endpoint:"ltc-usd/ticker", is_post:false}] }

import {url: "https://hooks.slack.com/services/T74RW7J0N/B891X5YNN/", key:"", secret:"", header:"",
	endpoints:[{fnName:"sendSlackMsg", endpoint:"BaQHlflLTmQQNKHH3EE6PrR1", is_post:true}] }

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

processMsg arg : Arr -> Obj
  count = 0
  prices = []
  price = 0
  foreach x in arg
    if count==0
      count = count+1
    else
      if count==1
        count = count+1
      else
        if(equals [x, "average"])
          price = getAvgPrice ""
        else
          if(equals [x, "litecoin"])
            price = litecoin ""
          else
            if(equals [x, "ethereum"])
              price = ether ""
            else
              if(equals [x, "bitcoin"])
                price = bitcoin ""
              else
                price = 0
                er = cat [x," not found"]
                log er
        sendtext = ""
        if(equals [x, "average"])
          sendtext = "bitcoin average price is $"
        else
          sendtext = cat [x, " price is $"]
        sendtext = cat [sendtext, price]
        prices = push [prices, sendtext]
        log sendtext
  st = ""
  foreach p in prices
    st = cat [st, p]
    st = cat [st, "\n"]
  if (equals [arg.[1], "slack"])
    js="{\"text\":\""
    js=cat [js,st]
    js=cat [js,"\"}"]
    payload = jn js
    sendSlackMsg payload
  else
    if (equals [arg.[1], "text"])
      payload = {}
      payload = addToObj [payload, "message", (cat [st,""])]
      sendJordanTxt payload
    else
      er = cat [arg.[1], " not found"]
      log er

helper getAvgPrice arg : Str -> Str
  gdaxprice = gdax arg
  cexprice = cex arg
  bitfinexprice = bitfinex arg
  bitstampprice = bitstamp arg
  arr = [gdaxprice, cexprice, bitfinexprice, bitstampprice]
  geminiprice = gemini arg
  arr = push [arr, geminiprice]
  average = avg arr
  x = cat ["",average]
  x

helper gdax arg : Str -> Num
  x = getBitcoinPrice arg
  res = jn x
  precio = (get [res, "price"])
  if isString precio
    precio = toNum precio
  else
    0
  precio

helper cex arg : Str -> Num
  x = cexBitcoinPrice arg
  res = jn x
  precio = get [res, "ask"]
  if isString precio
    precio = toNum precio
  else
    0
  precio

helper bitfinex arg : Str -> Num
  x = bitfinexBitcoinPrice arg
  res = jn x
  precio = res.[0]
  if isString precio
    precio = toNum precio
  else
    0
  precio

helper gemini arg : Str -> Num
  x = geminiBitcoinPrice arg
  res = jn x
  precio = get [res, "ask"]
  if isString precio
    precio = toNum precio
  else
    0
  precio

helper bitstamp arg : Str -> Num
  x = bitstampBitcoinPrice arg
  res = jn x
  precio = get [res, "ask"]
  if isString precio
    precio = toNum precio
  else
    0
  precio

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

