import
  leo scheme
  math

define n 10

pretty-print << calculating fibonacci >> n

time pretty-print << result is >>
  let fibonacci
    with number n
    if
      less?: number, 2
      then number
      else add
        fibonacci subtract: number, 1
        fibonacci subtract: number, 2
