/* Get the average of all numbers in an array.
   Input an array of numbers. */

avg arg : Arr -> Num
  count = 0
  total = 0
  foreach x in arg
    total = total + arg.[count]
    count = count + 1
  result = (total/count)
  result


/* Concatenate two arrays. Input an array of 
   two arrays. */

arrconcat arg : Arr -> Arr
  g = arg.[0]
  q = arg.[1]
  combo = []
  foreach x in g
    combo = push [combo, x]
  foreach y in q
    combo = push [combo, y]
  combo


/* Get the gcd of two numbers. Input is an
   array of two numbers. Not implemented yet.*/

gcd arg : Arr -> Num
  j = arg.[0]
  k = arg.[1]
  max = 0
  final = 0
  if (j < k)
    max = (k + 1)
  else
    max = (j + 1)
  
  arr = fixedArr max
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
fixedArr arg : Num -> Arr
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


/* Checks if an array contains a string or number. Takes
   in an array with two elements: the array to search and
   the string/number to search for. Returns a bool. */

contains arg : Arr -> Bool
  arr = arg.[0]
  focus = arg.[1]
  final = false
  arr = sort arr

  foreach i in arr
    if (isNum focus)
      if (isNum i)
        if (i == focus)
          final = true
        else
          final = final
      else
        final = final
    else
      final = final

    if (isString focus)
      if (isString i)
        if (equals [i, focus])
          final = true
        else
          final = final
      else
        final = final
    else 
      final = final
  log final
  final


/* Sorts an array of numbers. Takes in an array of two arrays, both being 
   all integers, and returns a sorted version of the array */

sort arr : Arr -> Arr
  length = 0
  temp = 0
  foreach i in arr
    length = (length + 1)
  ref = fixedArr length

  foreach i in ref
    inner = fixedArr (length - i - 1)
    foreach j in inner
      if (arr.[j] > arr.[j+1])
        temp = arr.[j]
        arr = update [arr, arr.[j + 1], j]
        arr = update [arr, temp, j + 1]
      else
        arr = arr
  arr
