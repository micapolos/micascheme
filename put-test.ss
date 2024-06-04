(import (scheme) (check) (put))

(let ()
  (define-put-syntax db
    (lambda ($port $syntax)
      (syntax-case $syntax ()
        ((_ $expr)
          #`(put-u8 $port $expr)))))
  (check (equal? (bytevector! (db #x10)) (bytevector #x10))))

(let ()
  (define-put-syntax db
    (lambda ($port $syntax)
      (lambda ($lookup)
        (syntax-case $syntax ()
          ((_ $expr)
            #`(put-u8 $port $expr))))))
  (check (equal? (bytevector! (db #x10)) (bytevector #x10))))

(let ()
  (define-put-syntax (db $port $syntax)
    (syntax-case $syntax ()
      ((_ $expr)
        #`(put-u8 $port $expr))))
  (check (equal? (bytevector! (db #x10)) (bytevector #x10))))

(let ()
  (define-put-syntax (db $port $syntax $lookup)
    (syntax-case $syntax ()
      ((_ $expr)
        #`(put-u8 $port $expr))))
  (check (equal? (bytevector! (db #x10)) (bytevector #x10))))

(let ()
  (define-put (db $expr)
    (u8 $expr)
    (u8 (add1 $expr)))
  (check (equal? (bytevector! (db #x10)) (bytevector #x10 #x11))))

(check (equal? (bytevector!) (bytevector)))
(check (equal? (bytevector! (u8 #x10) (u8 #x20)) (bytevector #x10 #x20)))
(check (equal? (bytevector! (u16-le #x1234)) (bytevector #x34 #x12)))
(check (equal? (bytevector! (u16-be #x1234)) (bytevector #x12 #x34)))
(check (equal? (bytevector! (utf8 "foo")) (string->utf8 "foo")))
(check (equal? (bytevector! (c-string "foo")) (string->utf8 "foo\x0;")))
(check (equal? (bytevector! (file "put-foo.txt")) (string->utf8 "foo\n")))

