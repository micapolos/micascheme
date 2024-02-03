(import
  (micascheme)
  (check)
  (zexy env)
  (zexy math))

(check
  (equal?
    (env... (foo 10) (bar 20))
    (env (stack (cons 'foo 10) (cons 'bar 20)))))

(lets
  ($env (env... (foo 10) (bar 20) (foo 30)))
  (run
    (check (equal? (env-get $env 'foo) 30))
    (check (equal? (env-get $env 'bar) 20))
    (check (equal? (env-get $env 'zoo) #f))

    (check (equal? (env-eval $env #'10) 10))
    (check (equal? (env-eval $env #'foo) 30))
    (check (equal? (env-eval $env #'bar) 20))
    (check (equal? (env-eval $env #'(- 30 20)) (- 30 20)))
    (check (equal? (env-eval $env #'(+ foo bar)) (+ 30 20)))
    (check (equal? (env-eval $env #'(- foo bar)) (- 30 20)))
    (check (equal? (env-eval $env #'(* foo bar)) (* 30 20)))
    (check (equal? (env-eval $env #'(shl foo 2)) (shl 30 2)))))
