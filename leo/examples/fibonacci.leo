import
  leo scheme
  leo math

define n 45
pretty-print quasiquote calculating fibonacci unquote n
pretty-print time
  let fibonacci
    with number n
    if
      less?: number, 2
      number
      add
        fibonacci subtract: number, 1
        fibonacci subtract: number, 2
