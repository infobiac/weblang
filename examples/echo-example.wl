fetch arg : Str -> Str
  //clientPost {url:"35.194.4.65:8000/echo", payload:"post1" }
  //clientPost {url:"35.194.4.65:8000/echo", payload:"post2" }

  clientPost {url:"35.194.4.65:8000/echo", payload:"julian" }

  test = clientGet {url:"api.gdax.com/products/eth-usd/ticker", body:""}
  log test
