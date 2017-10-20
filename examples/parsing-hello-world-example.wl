/* comment */

main arg : inType -> outType
  log "Hello,"
  log "world!"

otherFunction arg : someType -> someOtherType
  a = "a string with \"quotes\""
  b = a (nested (function "call")) // a comment!
  ["a top level", nested ["array"]]
  // another comment
  var = { some : key "value", pairs:2.0,complex:fn ["one"] }
  /* a bigger
     comment */
  log "this won't be called, because it's not main"
