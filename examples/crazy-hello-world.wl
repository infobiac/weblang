includes "../another_file.wl"

/* comment */

type Integral x : Number[integral]

type Natural x : Integral
  x > 0

x = ["first", 2, [3.0], {four: 4}]

main arg : inType -> outType
  log "hello world"
  [log "hello world", {hi: log "hello world"}]

otherFunction arg : someType -> someOtherType
  a = "a string with \"quotes\""
  b = a (nested (function "call")) // a comment!
  ["a top level", nested ["array"]]
  // another comment
  var = { some : key "value", pairs:2.0,complex:fn ["one"] }
  /* a bigger
     comment */
  log "this won't be called, because it's not main"
