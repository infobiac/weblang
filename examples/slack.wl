import {url: "https://hooks.slack.com/services/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",
	endpoints:[{name:"sendSlackMsg", is_post:true}] }

slack arg : Str -> Obj
  js = jn "{\"url\":\"https://hooks.slack.com/services/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\" }"
  args = addToObj [js, "payload", arg]
  sendSlackMsg args
