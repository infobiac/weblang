import {url: "https://hooks.slack.com/services/T74RW7J0N/B891X5YNN/",
	endpoints:[{fnName:"sendSlackMsg", endpoint:"BaQHlflLTmQQNKHH3EE6PrR1", is_post:true}] }

import {url: "https://api.sendgrid.com/api/",endpoints:[{fnName:"email", endpoint:"mail.send.json", is_post:true}] }

sendEmail arg : Str -> Obj
  empty = jn "{}"
  apiuser = addToObj [empty, "'apiuser'", "'weblang_julian'"]
  apikey = addToObj [apiuser, "'apikey'", "'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'"]
  to = addToObj [apikey, "'to'", "'jserra17@cmc.edu'"]
  from = addToObj [to, "'from'", "'weblang@stopthemelt.com'"]
  subject = addToObj [from, "'subject'", "'webLang, the best programming language'"]
  text = addToObj [subject, "'text'",arg]
  log text
  x =  email text
  log x
