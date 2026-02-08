(import
  (only (micascheme) check equal? quote procedure?)
  (micalang runtime)
  (prefix (micalang term) %))

(check
  (equal?
    (app
      (app
        (lambda a (lambda b (native (%%+ a b))))
        (native 10))
      (native 20))
    (native 30)))

(check
  (equal?
    (constant (this is my (constant)))
    #f))

(check
  (equal?
    (tagged (constant fx) (native 10))
    10))

(check (equal? (app zero? (native 0)) #t))

(check (equal? (app (app = (native 2)) (native 2)) (native #t)))
(check (equal? (app (app = (native 2)) (native 3)) (native #f)))
(check (equal? (app (app + (native 2)) (native 3)) (native 5)))
(check (equal? (app (app - (native 5)) (native 3)) (native 2)))
(check (equal? (app (app < (native 2)) (native 3)) (native #t)))

(check (procedure? (lambda x x)))
(check (equal? ((lambda x x) (native 10)) 10))

(check (procedure? (pi number boolean)))
(check (equal? ((pi number boolean) (native 10)) boolean))

(check (procedure? (pi (x number) (app (app + x) (native 1)))))
(check (equal? ((pi (x number) (app (app + x) (native 1))) (native 10)) 11))

(check (procedure? (pi (x type) x)))
(check (equal? ((pi (x type) x) number) number))

(check
  (equal?
    (let
      (x (native 10))
      (y (native 20))
      ((app + x) y))
    (native 30)))

(check
  (equal?
    (if (native #t) (native 10) (native 20))
    (native 10)))

(check
  (equal?
    (if (native #f) (native 10) (native 20))
    (native 20)))
