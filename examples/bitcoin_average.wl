include "examples/coin_helpers.wl"

helper getAvgPrice arg : Str -> Str
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


avg arg : Arr -> Num
  count = 0
  total = 0
  foreach x in arg
    total = total + arg.[count]
    count = count + 1
  result = (total/count)
