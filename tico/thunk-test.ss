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

(check
  (equal?
    (thunk-values-app-datum (thunk (arity 3) '(foo)))
    '(3 (foo))))

(check
  (equal?
    (thunks-values-app-datum
      (list
        (thunk (arity 0) '(foo))
        (thunk (arity 1) '(bar))
        (thunk (arity 2) '(goo))))
    '(values-app
      (0 (foo))
      (1 (bar))
      (2 (goo)))))

(check
  (equal?
    (thunks-app-datum-opt
      (list
        (thunk (arity 0) '(foo))
        (thunk (arity 1) '(bar))
        (thunk (arity 2) '(goo))))
    #f))

(check
  (equal?
    (thunks-app-datum-opt
      (list
        (thunk (arity 1) '(foo))
        (thunk (arity 1) '(bar))
        (thunk (arity 1) '(goo))))
    '(app (foo) (bar) (goo))))

(check
  (equal?
    (thunks-app-datum
      (list
        (thunk (arity 0) '(foo))
        (thunk (arity 1) '(bar))
        (thunk (arity 2) '(goo))))
    (thunks-values-app-datum
      (list
        (thunk (arity 0) '(foo))
        (thunk (arity 1) '(bar))
        (thunk (arity 2) '(goo))))))

(check
  (equal?
    (thunks-app-datum-opt
      (list
        (thunk (arity 1) '(foo))
        (thunk (arity 1) '(bar))
        (thunk (arity 1) '(goo))))
    (thunks-app-datum-opt
      (list
        (thunk (arity 1) '(foo))
        (thunk (arity 1) '(bar))
        (thunk (arity 1) '(goo))))))

(check
  (equal?
    (thunk-application
      (arity 10)
      (thunk (arity 0) '(foo))
      (list
        (thunk (arity 1) '(bar))
        (thunk (arity 2) '(goo))))
    (thunk
      (arity 10)
      (thunks-app-datum
        (list
          (thunk (arity 0) '(foo))
          (thunk (arity 1) '(bar))
          (thunk (arity 2) '(goo)))))))

(check
  (equal?
    (thunk-application
      (arity 10)
      (thunk (arity 1) '(foo))
      (list
        (thunk (arity 1) '(bar))
        (thunk (arity 1) '(goo))))
    (thunk
      (arity 10)
      (thunks-app-datum
        (list
          (thunk (arity 1) '(foo))
          (thunk (arity 1) '(bar))
          (thunk (arity 1) '(goo)))))))
