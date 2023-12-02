(import
  (micascheme)
  (tico argument))

(lets
  ($argument (argument 'foo "foo"))
  (run
    (check (argument? $argument))
    (check (not (argument? "foo")))
    (check (equal? (argument-key $argument) 'foo))
    (check (equal? (argument-value $argument) "foo"))))
