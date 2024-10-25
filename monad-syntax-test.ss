(import (scheme) (syntax) (monad-syntax) (check))

; === pure ===

(let ()
  (define-aux-keyword option)
  (define-pure option (lambda ($value) $value))
  (check (procedure? (pure option)))
  (check (equal? ((pure option) 123) 123))
  (check (equal? (pure option 123) 123)))

(let ()
  (define-aux-keyword option)
  (define-pure (option $value) $value)
  (check (procedure? (pure option)))
  (check (equal? ((pure option) 123) 123))
  (check (equal? (pure option 123) 123)))

; == bind ===

(let ()
  (define-aux-keyword option)
  (define-bind option
    (lambda ($fn $value)
      (and $value ($fn $value))))
  (check (procedure? (bind option)))
  (check (equal? ((bind option) fx1+ 123) 124))
  (check (equal? (bind option fx1+ 123) 124))
  (check (equal? (bind option fx1+ #f) #f)))

(let ()
  (define-aux-keyword option)
  (define-bind (option $fn $value)
    (and $value ($fn $value)))
  (check (procedure? (bind option)))
  (check (equal? ((bind option) fx1+ 123) 124))
  (check (equal? (bind option fx1+ 123) 124))
  (check (equal? (bind option fx1+ #f) #f))
  (check (equal? (bind (option fx 123) (fx1+ fx)) 124))
  (check (equal? (bind (option fx #f) (fx1+ fx)) #f)))

; === fmap ===

(let ()
  (define-rule-syntax (io body) (lambda () body))
  (define (io-run $io) ($io))
  (define-pure (io $value) (io $value))
  (define-bind (io $fn $io) ($fn (io-run $io)))

  (check (procedure? (fmap io)))
  (check (equal? (io-run ((fmap io) fx1+ (io 123))) 124))
  (check (equal? (io-run (fmap io fx1+ (io 123))) 124))
  (check (equal? (io-run (fmap (io fx (io 123)) (fx1+ fx))) 124)))

; === flat-map ===

(let ()
  (define-rule-syntax (io body) (lambda () body))
  (define (io-run $io) ($io))
  (define-pure (io $value) (io $value))
  (define-bind (io $fn $io) ($fn (io-run $io)))

  (check (procedure? (flat-map io)))
  (check (equal? (io-run ((flat-map io) (list (io 1) (io 2)))) (list 1 2)))
  (check (equal? (io-run (flat-map io (list (io 1) (io 2)))) (list 1 2))))
