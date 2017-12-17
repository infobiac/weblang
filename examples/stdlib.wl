/* Get the average of all numbers in an array.
   Input an array of numbers. */

avg arg : Str -> Num
  g = jn arg
  count = 0
  total = 0
  foreach x in g
    total = total + g.[count]
    count = count + 1
  result = (total/count)


/* Concatenate two arrays. Input an array of 
   two arrays. */

arrconcat arg : Str -> Obj
  r = jn arg
  g = r.[0]
  q = r.[1]
  combo = []
  foreach x in g
    combo = push [combo, x]
  foreach y in q
    combo = push [combo, y]
  log combo


/* Get the gcd of two numbers. Input is an
   array of two numbers. Not implemented yet.*/

gcd arg : Str -> Num
  array = jn arg
  j = array.[0]
  k = array.[1]
  max = 0
  final = 0
  if (j < k)
    max = (k + 1)
  else
    max = (j + 1)
  
  arr = createFixedArr max
  foreach i in arr
    if (k%i == 0)
      if (j%i == 0)
        final = i
      else
        final = final
    else
      final = final

  log final
  final

/* This function is used to create an array of
   a particular size. Most practical use case is
   for turning foreach into more of the python
   "for i in range x" by creating a dummy array of
   size x.
*/
createFixedArr arg : Num -> Arr
  num = arg
  arr = []
  pass = []
  pass = push [pass, arr]
  pass = push [pass, num]
  pass = push [pass, 0]
  final = []

  if (num == 0)
    final = arr
  else
    final = createArrRec pass

  final


createArrRec arg : Arr -> Arr
  arr = arg.[0]
  num = arg.[1]
  count = arg.[2]
  ret = []

  if ( num == 0 )
    ret = arr
  else
    arr = push [arr, count]
    num = (num - 1)
    count = (count + 1)
    ret = createArrRec [arr, num, count]

  ret
  
