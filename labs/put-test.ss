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

(define-put-syntax (file $port $syntax)
  (syntax-case $syntax ()
    ((_ $path)
      #`(call-with-port (open-file-input-port $path)
        (lambda ($input)
          (put-bytevector $port (get-bytevector-all $input)))))))

(check (equal? (bytevector!) (bytevector)))
(check (equal? (bytevector! (u8 #x10) (u8 #x20)) (bytevector #x10 #x20)))
(check (equal? (bytevector! (u16 #x1234)) (bytevector #x34 #x12)))
(check (equal? (bytevector! (utf8 "foo")) (string->utf8 "foo")))
(check (equal? (bytevector! (c-string "foo")) (string->utf8 "foo\x0;")))
(check (equal? (bytevector! (file "labs/put-foo.txt")) (string->utf8 "foo\n")))
