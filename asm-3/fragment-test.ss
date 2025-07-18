(import (micascheme) (asm-3 expression) (asm-3 fragment) (syntax lookup))

(check
  (equal?
    (fragment->bytevector 100 (lookup-with) (db 10 20 30))
    (bytevector 10 20 30)))

(check
  (equal?
    (fragment->bytevector 100 (lookup-with) (db org))
    (bytevector 100)))

(check
  (equal?
    (fragment->bytevector 100 (lookup-with (foo 10)) (db foo))
    (bytevector 10)))

(check
  (equal?
    (fragment->bytevector 100 (lookup-with (+ +) (foo 10) (bar 20)) (db (+ foo bar)))
    (bytevector 30)))
