(import (scheme) (check) (labs put))

(define-put-syntax u8
  (lambda ($port $syntax)
    (syntax-case $syntax ()
      ((_ $expr)
        #`(put-u8 $port $expr)))))

(define-put-syntax (u16 $port $syntax)
  (syntax-case $syntax ()
    ((_ $expr)
      #`(let (($u16 $expr))
        (put-u8 $port (fxand $u16 #xff))
        (put-u8 $port (fxsrl $u16 8))))))

(define-put-syntax (utf8 $port $syntax)
  (syntax-case $syntax ()
    ((_ $expr)
      #`(put-bytevector $port (string->utf8 $expr)))))

(define-put (c-string $expr)
  (utf8 $expr)
  (u8 0))

(check
  (equal?
    (bytevector!
      (u8 #x10)
      (u8 #x20)
      (u16 #x1234)
      (utf8 "foo")
      (c-string "foo"))
    (bytevector
      #x10
      #x20
      #x34 #x12
      (char->integer #\f) (char->integer #\o) (char->integer #\o)
      (char->integer #\f) (char->integer #\o) (char->integer #\o) 0)))
