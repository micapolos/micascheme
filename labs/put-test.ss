(import (scheme) (check) (labs put))

(define-put-syntax (u8 $port $syntax)
  (syntax-case $syntax ()
    ((_ $expr)
      #`(put-u8 $port $expr))))

(define-put-syntax (u16 $port $syntax)
  (syntax-case $syntax ()
    ((_ $expr)
      #`(let (($u16 $expr))
        (put-u8 $port (fxand $u16 #xff))
        (put-u8 $port (fxsrl $u16 8))))))

(check
  (equal?
    (bytevector! (u8 #x10) (u8 #x20) (u16 #x1234))
    (bytevector #x10 #x20 #x34 #x12)))
