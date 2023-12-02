(import
  (micascheme)
  (tico argument))

(lets
  ($argument (argument (list 'foo 'bar) '(values "foo" "bar")))
  (run
    (check (argument? $argument))
    (check (not (argument? "foo")))
    (check (equal? (argument-key $argument) (list 'foo 'bar)))
    (check (equal? (argument-value $argument) '(values "foo" "bar")))))
