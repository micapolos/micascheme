(import
  (except (micascheme) pi)
  (micalang term)
  (micalang compiler))

; === int

(check
  (typed-equal?
    (mica-compile #t '() 123)
    (typed (native 'int) 123)))

(check
  (raises
    (mica-compile #t '() 123123123123123123123123)))

; === variable

(check
  (typed-equal?
    (mica-compile #t
      `(
        (x1 ,(typed (native 't1) 'v1))
        (x2 ,(typed (native 't2) 'v2)))
      'x1)
    (typed (native 't1) 'v1)))

(check
  (typed-equal?
    (mica-compile #t
      `(
        (x1 ,(typed (native 't1) 'v1))
        (x2 ,(typed (native 't2) 'v2)))
      'x2)
    (typed (native 't2) 'v2)))

(check
  (raises
    (mica-compile #t
      `(
        (x1 ,(typed 't1 'v1))
        (x2 ,(typed 't2 'v2)))
      'x3)))

; === lambda

; (check
;   (typed-equal?
;     (mica-compile #t
;       `(
;         (int ,(typed (native 'type) '(native 'int))))
;       '((lambda (x : int) x) 123))
;     (typed (native 'int) 123)))


