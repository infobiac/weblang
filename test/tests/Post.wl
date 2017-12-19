import {url: "https://hooks.slack.com/services/T74RW7J0N/B891X5YNN/", key: "", secret:"", header:"", 
	endpoints:[{fnName:"sendSlackMsg", endpoint:"BaQHlflLTmQQNKHH3EE6PrR1", is_post:true}] }

testPost arg : Str -> Obj
  body = {}
  body = addToObj [body, "text", "Running test suite"]
  body = addToObj [body, "channel", "#testing"]
  x = sendSlackMsg body
  log x
  body
