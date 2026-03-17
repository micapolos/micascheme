import
  leo scheme
  math

define n 45

write quasiquote calculating fibonacci unquote n

write time
  let fibonacci
    with number n
    if
      less?: number, 2
      then number
      else add
        fibonacci subtract: number, 1
        fibonacci subtract: number, 2
