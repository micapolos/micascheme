(import
  (only (micascheme) check equal? quote procedure?)
  (prefix (micalang term) %)
  (micalang comptime))

(check (equal? any-type any-type))
(check (equal? any-boolean (%native 'any-boolean)))
(check (equal? any-number (%native 'any-number)))
(check (equal? any-symbol (%native 'any-symbol)))
(check (equal? any-char (%native 'any-char)))
(check (equal? any-string (%native 'any-string)))

(check
  (equal?
    (constant (this is my (constant)))
    (%constant '(this is my (constant)))))

(check
  (equal?
    (tagged (constant fx) (native 10))
    (%tagged (%constant 'fx) (%native 10))))

(check
  (equal?
    ((%macro-procedure (macro ($compiler $term) $term)) 'comp 123)
    123))

(check (equal? (app zero? (native 0)) (%native #t)))

(check (equal? (app (app = (native 2)) (native 2)) (%native #t)))
(check (equal? (app (app = (native 2)) (native 3)) (%native #f)))
(check (equal? (app (app + (native 2)) (native 3)) (%native 5)))
(check (equal? (app (app - (native 5)) (native 3)) (%native 2)))
(check (equal? (app (app < (native 2)) (native 3)) (%native #t)))

(check (equal? (pi-param (any-lambda any-number any-boolean)) any-number))
(check (equal? (app (any-lambda any-number any-boolean) (native '())) any-boolean))

(check (equal? (pi-param (any-lambda (x any-type) x)) any-type))
(check (equal? (app (any-lambda (x any-type) x) any-number) any-number))

(check
  (equal?
    (if (native #t) (native 10) (native 20))
    (%native 10)))

(check
  (equal?
    (if (native #f) (native 10) (native 20))
    (%native 20)))

(check
  (equal?
    (let (x (native 10))
      (let (y (native 20))
        (app (app + x) y)))
    (%native 30)))
