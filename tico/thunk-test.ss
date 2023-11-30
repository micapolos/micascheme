(import
  (micascheme)
  (tico thunk))

(lets
  ($thunk (thunk (arity 1) '(string-append foo bar)))
  (run
    (check (thunk? $thunk))
    (check (not (thunk? 128)))
    (check (equal? (thunk-arity $thunk) (arity 1)))
    (check (equal? (thunk-datum $thunk) '(string-append foo bar)))))
