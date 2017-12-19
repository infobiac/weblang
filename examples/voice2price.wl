include "examples/bitcoin_average.wl"

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
    payload
  else
    if (equals [arg.[1], "text"])
      payload = {}
      payload = addToObj [payload, "message", (cat [st,""])]
      sendJordanTxt payload
      payload
    else
      er = cat [arg.[1], " not found"]
      log er
