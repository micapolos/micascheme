(import (micascheme) (asm-3 expression) (asm-3 fragment) (asm-3 dependent) (syntax lookup))

(check
  (equal?
    (fragment->bytevector
      (lookup-with)
      100
      (dependent-ref (db 10 20 30)))
    (bytevector 10 20 30)))

(check
  (equal?
    (fragment->bytevector
      (lookup-with)
      100
      (dependent-ref (db org)))
    (bytevector 100)))

(check
  (equal?
    (fragment->bytevector
      (lookup-with (foo 10))
      100
      (dependent-ref (db foo)))
    (bytevector 10)))

(check
  (equal?
    (fragment->bytevector
      (lookup-with (+ +) (foo 10) (bar 20))
      100
      (dependent-ref (db (+ foo bar))))
    (bytevector 30)))
