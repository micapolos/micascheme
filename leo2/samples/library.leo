stop "!"
separator ", "
lucky 7

doubly lucky:
  the: added
    the lucky number
    the lucky number

greeting: with
  any string
  giving: the: appended
    "Hello"
    the separator string
    the string
    the stop string (default "!")

hello world:
  as string
  the greeting "world"

hello hell:
  as string
  the greeting "hell" (stop "?")

increment:
  as number
  with
    any number
    giving: the: sum
      of (the number)
      and 1

decrement:
  as number
  with
    any number
    giving: the: difference
      between (the number)
      and 1
