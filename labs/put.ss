(library (labs put)
  (export
    define-put-syntax
    put
    bytevector!)
  (import (scheme) (syntax) (syntaxes) (lets))

  (define-rules-syntaxes
    ((define-put-syntax ($id $port $syntax) $body ...)
      (define-put-syntax ($id $port $syntax $lookup) $body ...))
    ((define-put-syntax ($id $port $syntax $lookup) $body ...)
      (begin
        (define-aux-keyword $id)
        (define-property $id put
          (lambda ($port $syntax $lookup)
            $body ...)))))

  (define-lookup-syntax (put $syntax $lookup)
    (syntax-case $syntax ()
      ((_ $port-expr $op ...)
        (with-implicit (put $port)
          (define (parse-op $syntax)
            (syntax-case $syntax ()
              (($op $arg ...)
                ($lookup #'$op #'put)
                (($lookup #'$op #'put) #'$port $syntax $lookup))))
          #`(let ()
            (define $port $port-expr)
            #,@(map parse-op (syntax->list #'($op ...)))
            (void))))))

  (define-rule-syntax (bytevector! $op ...)
    (call-with-bytevector-output-port
      (lambda ($port)
        (put $port $op ...))))
)
