(library (labs put)
  (export
    define-put-syntax
    define-put
    put
    bytevector!)
  (import (scheme) (syntax) (syntaxes) (lets))

  (define-rules-syntaxes
    ((define-put-syntax $id $expr)
      (identifier? #'$id)
      (begin
        (define-aux-keyword $id)
        (define-property $id put $expr)))
    ((define-put-syntax ($id $port $syntax) $body ...)
      (define-put-syntax $id
        (lambda ($port $syntax) $body ...)))
    ((define-put-syntax ($id $port $syntax $lookup) $body ...)
      (define-put-syntax $id
        (lambda ($port $syntax)
          (lambda ($lookup)
            $body ...)))))

  (define-rules-syntaxes
    ((define-put ($id $arg ...) $body ...)
      (define-put-syntax ($id $port $syntax)
        (syntax-case $syntax ()
          ((_ $arg ...)
            #`(begin (put #,$port $body) ...))))))

  (define-lookup-syntax (put $syntax $lookup)
    (syntax-case $syntax ()
      ((_ $port-expr $op ...)
        (with-implicit (put $port)
          (define (parse-op $syntax)
            (syntax-case $syntax (put)
              (($op $arg ...)
                ($lookup #'$op #'put)
                (let (($transformer (($lookup #'$op #'put) #'$port $syntax)))
                  (if (procedure? $transformer)
                    ($transformer $lookup)
                    $transformer)))))
          #`(let ()
            (define $port $port-expr)
            #,@(map parse-op (syntax->list #'($op ...)))
            (void))))))

  (define-rule-syntax (bytevector! $op ...)
    (call-with-bytevector-output-port
      (lambda ($port)
        (put $port $op ...))))
)
