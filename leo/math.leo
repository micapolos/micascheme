library
  leo math
  export
    add, subtract, multiply, divide
    less?, less/equal?, greater?, greater/equal?
    fx-add/wraparound, fx-subtract/wraparound, fx-less?
  import leo lang

  define fx-add/wraparound fx+/wraparound
  define fx-subtract/wraparound fx-/wraparound
  define fx-less? fx<

  define add +
  define subtract -
  define multiply *
  define divide /
  define less? <
  define less/equal? <=
  define greater? >
  define greater/equal? >=
