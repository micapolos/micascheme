(import (scheme) (syntax) (monad-syntax) (check))

(define-aux-keyword option)

; === define-pure-syntax ===

(let ()
  (define-pure-syntax option
    (lambda ($syntax)
      (syntax-case $syntax ()
        ((_ body ...) #`(let () body ...)))))

  (check (equal? (pure option 123) 123)))

; === define-pure ===

(let ()
  (define-pure (option $value) $value)
  (check (equal? (pure option 123) 123)))

(let ()
  (define-pure option (lambda ($value) $value))
  (check (equal? (pure option 123) 123)))

; === define-bind-syntax ===

(let ()
  (define-bind-syntax option
    (lambda ($syntax)
      (syntax-case $syntax ()
        ((_ (var expr) body ...)
          #`(let ((var expr))
            (and var (let () body ...)))))))
  (check (equal? (bind option (x 128) (+ x 1)) 129))
  (check (equal? (bind option (x 128) #f) #f))
  (check (equal? (bind option (x #f) (+ x 1)) #f))
  (check (equal? (bind option (x #f) #f) #f)))

; === define-bind ===

(let ()
  (define-bind (option $value $fn)
    (and $value ($fn $value)))
  (check (equal? (bind option (x 128) (+ x 1)) 129))
  (check (equal? (bind option (x 128) #f) #f))
  (check (equal? (bind option (x #f) (+ x 1)) #f))
  (check (equal? (bind option (x #f) #f) #f))

  (check
    (equal?
      (bind option
        (x 10)
        (y (+ x x))
        (* x y))
      200)))
