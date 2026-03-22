import leo scheme

define n 10

pretty-print << calculating fibonacci >> n

time pretty-print << result is >>
  let fibonacci
    with number n
    if
      less?: number, 2
      number
      add
        fibonacci subtract: number, 1
        fibonacci subtract: number, 2
