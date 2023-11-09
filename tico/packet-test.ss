(import
  (micascheme)
  (tico packet))

(check
  (equal?
    (datum->packet '(+ 1 2))
    (packet '(+ 1 2) 3)))

(check
  (equal?
    (literal->packet "foo")
    (packet "foo" "foo")))

(check
  (equal?
    (test-packet foo)
    (packet ''foo 'foo)))

