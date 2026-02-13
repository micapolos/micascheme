(import
  (only (micascheme) check equal? quote procedure?)
  (prefix (micalang term) %)
  (micalang comptime))

(check (equal? a-type a-type))
(check (equal? a-boolean a-boolean))
(check (equal? a-number a-number))
(check (equal? a-symbol a-symbol))
(check (equal? a-char a-char))
(check (equal? a-string a-string))

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

(check (equal? (type-abstraction-param (a-lambda a-number a-boolean)) a-number))
(check (equal? (app (a-lambda a-number a-boolean) (native '())) a-boolean))

(check (equal? (type-abstraction-param (a-lambda (x a-type) x)) a-type))
(check (equal? (app (a-lambda (x a-type) x) a-number) a-number))

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
