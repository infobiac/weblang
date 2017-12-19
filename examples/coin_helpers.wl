include "examples/coin_imports.wlh"

bitcoin arg : Str -> Str
  x = getBitcoinPrice arg
  res = jn x
  precio = (get [res, "price"])
  precio

ether arg : Str -> Str
  x = getEtherPrice arg
  res = jn x
  precio = (get [res, "price"])
  precio

litecoin arg : Str -> Str
  x = getLitecoinPrice arg
  res = jn x
  precio = (get [res, "price"])
  precio

