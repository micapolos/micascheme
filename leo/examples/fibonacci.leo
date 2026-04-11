import leo scheme

define n 10

print `calculating fibonacci` n

time print `result is`
  let fibonacci
    with number n
    if
      less?: number, 2
      number
      add
        fibonacci subtract: number, 1
        fibonacci subtract: number, 2
