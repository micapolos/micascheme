(import
  (only (micascheme) check equal? bytevector lambda)
  (asm-2 block-fragment)
  (asm-2 fragment)
  (asm-2 core)
  (syntax lookup))

(check
  (equal?
    (fragment-ref (wrap "foo") (empty-lookup))
    "foo"))

(check
  (equal?
    (fragment-ref (wrap a) (lookup-with (a "foo")))
    "foo"))

(check
  (equal?
    (fragment->bytevector
      (db 10 20 30)
      (empty-lookup)
      #xc000)
    (bytevector 10 20 30)))

(check
  (equal?
    (fragment->bytevector
      (db a)
      (lookup-with (a 10))
      #xc000)
    (bytevector 10)))

(check
  (equal?
    (fragment->bytevector
      (db (+ 10 20))
      (empty-lookup)
      #xc000)
    (bytevector 30)))

; (check
;   (equal?
;     (fragment->bytevector
;       (block
;         start
;         (db #x12)
;         (dw #x3456)
;         (dw start))
;       (empty-lookup)
;       #xc000)
;     (bytevector #x12 #x56 #x34 #x00 #xc0)))
