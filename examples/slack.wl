import {url: "https://hooks.slack.com/services/T74RW7J0N/B891X5YNN/", key: "", secret:"",
	endpoints:[{fnName:"sendSlackMsg", endpoint:"BaQHlflLTmQQNKHH3EE6PrR1", is_post:true}] }

slack arg : Str -> Obj
  sendSlackMsg {text: arg}
