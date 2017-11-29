main arg : inType -> outType
  clientPost "{\"url\":\"35.194.4.65:8000/echo\", \"body\":\"post1\" }"
  clientPost "{\"url\":\"35.194.4.65:8000/echo\", \"body\":\"post2\" }"

  clientPost "{\"url\":\"35.194.4.65:8000/echo\", \"body\":\"lizzie\" }"
  log "Hello"
