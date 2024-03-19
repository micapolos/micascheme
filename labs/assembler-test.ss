(import (check) (labs assembler))

(check (equal? (assemble) #vu8()))
(check (equal? (assemble (db 10)) #vu8(10)))
(check (equal? (assemble (db 10) (db 20)) #vu8(10 20)))

(check (equal? (assemble (db)) #vu8()))
(check (equal? (assemble (db 10)) #vu8(10)))
(check (equal? (assemble (db 10 20)) #vu8(10 20)))

(define-assemble-syntax dw
  (lambda ($syntax)
    (syntax-case $syntax ()
      ((_ $expr)
        #`(let (($u16 $expr))
          (db (fxand $u16 8))
          (db (fxsrl $u16 8)))))))

(check (equal? (assemble (dw #x1234)) #vu8(20)))
