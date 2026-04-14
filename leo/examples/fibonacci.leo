import leo scheme

define n string->number car with command-line-arguments

print `calculating fibonacci` n

time print `result is`
  let fibonacci
    number n
    in if
      fx<: number, 2
      number
      fx+/wraparound
        fibonacci fx-/wraparound: number, 1
        fibonacci fx-/wraparound: number, 2
