def fact(x):
	ret = 1
	if x >= 1:
		for i in range(1,x+1):
			ret = ret * i
	return ret

def fib(x):
  if x < 2:
    return 1
  else:
    return fib(x-1) + fib(x-2)

def wiggle(z):
 return JsonResponse({
  "wiggle": 5,
  # this is really important
  "wobble: 6"
 })

def main():
    "This is a comment"
    """This is also a comment"""
    """This
       =
       comment
    """
    """
       This
       =
       comment
    """

    print(fib(5))
    print(fact(7))

if 1 == 2:
   main()
else:
     main()
