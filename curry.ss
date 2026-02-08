(library (curry)
  (export curry curry< curry+ curry-)
  (import (scheme))

  (define-syntax (curry $syntax)
    (syntax-case $syntax ()
      ((id 0 proc) #'proc)
      ((id n proc)
        (let ((arity (datum n)))
          (let f ((i arity))
            (if (= i 0)
              #'proc
              (with-syntax
                ((rest (f (- i 1)))
                 (arg (datum->syntax #'id (gensym))))
                #'(lambda (arg) (rest arg)))))))))

  (define (curry< a)
    (lambda (b)
      (< a b)))

  (define (curry+ a)
    (lambda (b)
      (+ a b)))

  (define (curry- a)
    (lambda (b)
      (- a b)))
)
