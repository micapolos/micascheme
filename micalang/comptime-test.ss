(import
  (only (micascheme) check equal? quote procedure?)
  (prefix (micalang term) %)
  (micalang comptime))

(check (equal? (app (app (curry a b %%+) (native 10)) (native 20)) (native 30)))

(check
  (equal?
    (%abstraction-symbol (prim zero? a))
    'a))

(check
  (equal?
    (%abstraction-apply (prim string-length s) (native "foo"))
    (%native 3)))

(check
  (equal?
    (%abstraction-symbol (prim + a b))
    'a))

(check
  (equal?
    (%abstraction-symbol (%abstraction-apply (prim + a b) (native 2)))
    'b))

(check
  (equal?
    (%abstraction-apply (%abstraction-apply (prim + a b) (native 2)) (native 3))
    (%native 5)))

(check
  (equal?
    (app (app (prim + a b) (native 2)) (native 3))
    (%native 5)))

(check (equal? type (%native 'type)))
(check (equal? boolean (%native 'boolean)))
(check (equal? number (%native 'number)))
(check (equal? symbol (%native 'symbol)))
(check (equal? string (%native 'string)))

(check (equal? (app zero? (native 0)) (%native #t)))

(check (equal? (app (app = (native 2)) (native 2)) (%native #t)))
(check (equal? (app (app = (native 2)) (native 3)) (%native #f)))
(check (equal? (app (app + (native 2)) (native 3)) (%native 5)))
(check (equal? (app (app - (native 5)) (native 3)) (%native 2)))
(check (equal? (app (app < (native 2)) (native 3)) (%native #t)))

(check (equal? (pi-param (pi number boolean)) number))
(check (equal? (app (pi number boolean) (native '())) boolean))

(check (equal? (pi-param (pi (x type) x)) type))
(check (equal? (app (pi (x type) x) number) number))

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
