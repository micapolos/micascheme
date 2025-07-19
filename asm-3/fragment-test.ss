(import (micascheme) (asm-3 expression) (asm-3 fragment) (asm-3 dependent) (syntax lookup))

; db

(check
  (equal?
    (fragment->bytevector 100
      (lookup-with)
      (db))
    (bytevector)))

(check
  (equal?
    (fragment->bytevector 100
      (lookup-with)
      (db 10 20 30))
    (bytevector 10 20 30)))

(check
  (equal?
    (fragment->bytevector 100
      (lookup-with)
      (db org))
    (bytevector 100)))

(check
  (equal?
    (fragment->bytevector 100
      (lookup-with (foo 10))
      (db foo))
    (bytevector 10)))

(check
  (equal?
    (fragment->bytevector 100
      (lookup-with (+ +) (foo 10) (bar 20))
      (db (+ foo bar)))
    (bytevector 30)))

; dw

(check
  (equal?
    (fragment->bytevector #xc000
      (lookup-with)
      (dw))
    (bytevector)))

(check
  (equal?
    (fragment->bytevector #xc000
      (lookup-with)
      (dw #x0123))
    (bytevector #x23 #x01)))

(check
  (equal?
    (fragment->bytevector #xc000
      (lookup-with)
      (dw org))
    (bytevector #x00 #xc0)))

(check
  (equal?
    (fragment->bytevector #xc000
      (lookup-with (foo #x1234))
      (dw foo))
    (bytevector #x34 #x12)))
