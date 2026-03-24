import leo scheme

define n 10

write << calculating fibonacci >> n

time write << result is >>
  let fibonacci
    with number n
    if
      less?: number, 2
      number
      add
        fibonacci subtract: number, 1
        fibonacci subtract: number, 2
