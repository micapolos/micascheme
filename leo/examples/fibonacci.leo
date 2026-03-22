import leo scheme

define n 10

pretty-print << calculating fibonacci >> n

time pretty-print << result is >>
  let fibonacci
    with number n
    if
      <: number, 2
      number
      +
        fibonacci -: number, 1
        fibonacci -: number, 2
