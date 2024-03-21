(import (check) (zexy flattenize))

(define-syntax flattenized-list
  (lambda ($syntax)
    (syntax-case $syntax ()
      ((_ $body ...)
        #`(list
          #,@(flattenize
            (syntax->list #'($body ...))))))))

(check (equal? (flattenized-list) (list)))
(check (equal? (flattenized-list 10 20 30) (list 10 20 30)))
(check (equal? (flattenized-list (begin 10 20) 30) (list 10 20 30)))
(check (equal? (flattenized-list (begin (begin 10) (begin) (begin 20)) 30) (list 10 20 30)))
