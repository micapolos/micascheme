(import
  (tt lang)
  (tt datum)
  (tt list))

(check (datum=? '10 '10))
(check (datum=? (boolean->datum #f) '#f))
(check (datum=? (number->datum 10) '10))
(check (datum=? (char->datum #\a) '#\a))
(check (datum=? (string->datum "foo") '"foo"))
(check (datum=? (cons '"foo" '"bar") '("foo" . "bar")))

(check (datum=? (datum-append 'point '10 '20) '(point 10 20)))
(check (datum=? (list->datum (list 'point '10 '20)) '(point 10 20)))
