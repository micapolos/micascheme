(import (check) (procedure))

; === once-proc ===

(check
  (equal?
    (let (($fn (once-proc (lambda () 123)))) ($fn))
    123))

(check
  (raises?
    (lambda ()
      (let (($fn (once-proc (lambda () 123))))
        ($fn) ($fn)))))

; === checking-once ===

(check
  (equal?
    (let (($fn (checking-once 123))) ($fn))
    123))

(check
  (raises?
    (lambda ()
      (let (($fn (checking-once 123)))
      ($fn)
      ($fn)))))
