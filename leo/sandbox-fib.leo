import leo scheme

pretty-print quote calculating fibonacci 45
pretty-print time
  let fibonacci
    with number 45
    if
      <: number, 2
      number
      +
        fibonacci -: number, 1
        fibonacci -: number, 2
