(import
  (leo2 datum)
  (leo2 comptime)
  (leo2 stdlib))

(check-term->datum=?
  (string-term "foo")
  (native (native (type 0) a-string) "foo"))

(check-term->datum=?
  (string-term "foo")
  (native (native (type 0) a-string) "foo"))

(check-term->datum=?
  (symbolic-term 'pretty (variable-term string-type 'number))
  (symbolic pretty number))

(check-term->datum=?
  (indexed-term 10 (symbol-term 'thing))
  (indexed 10 thing))

(check-term->datum=?
  (symbol-term 'thing)
  thing)
