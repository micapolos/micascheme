import
  leo scheme
  leo examples math

define n 45

pretty-print quasiquote calculating fibonacci unquote n

pretty-print time
  let fibonacci
    with number n
    if
      less?: number, 2
      then number
      else add
        fibonacci subtract: number, 1
        fibonacci subtract: number, 2
