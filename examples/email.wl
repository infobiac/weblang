import {url: "https://hooks.slack.com/services/T74RW7J0N/B891X5YNN/", key:"", secret:"", header:"", endpoints:[{fnName:"sendSlackMsg", endpoint:"BaQHlflLTmQQNKHH3EE6PrR1", is_post:true}] }

import {url: "https://api.sendgrid.com/v3/", key:"", secret:"", header:"{'content-type':'application/json','authorization':'Bearer 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'", endpoints:[{fnName:"email", endpoint:"mail/send", is_post:true}] }

sendEmail arg : Str -> Obj
  empty = jn "{}"
  apiuser = addToObj [empty, "to", ""]
  apikey = addToObj [apiuser, "apikey", "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"]
  to = addToObj [apikey, "to", "jserra17@cmc.edu"]
  from = addToObj [to, "from", "weblang@stopthemelt.com"]
  subject = addToObj [from, "subject", "webLang, the best programming language"]
  text = addToObj [subject, "text",arg]
  log text 
  x =  email text
  log x
