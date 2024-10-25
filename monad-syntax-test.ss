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
  (check (equal? (bind option fx1+ #f) #f)))

; === fmap ===

(let ()
  (define-rule-syntax (io body) (lambda () body))
  (define (io-run $io) ($io))
  (define-pure (io $value) (io $value))
  (define-bind (io $fn $io) ($fn (io-run $io)))
  (check (equal? (io-run (io 123)) 123))
  (check (equal? (io-run (pure io 123)) 123))
  (check (equal? (io-run (bind io (lambda ($value) (io (fx1+ $value))) (io 123))) 124))
  (check (procedure? (fmap io)))
  (check (equal? (io-run ((fmap io) fx1+ (io 123))) 124))
  (check (equal? (io-run (fmap io fx1+ (io 123))) 124)))
