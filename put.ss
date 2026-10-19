(library (put)
  (export
    define-put-syntax
    define-put
    put
    bytevector!
    u8
    u16-le u16-be
    u32-le u32-be
    u64-le u64-be
    utf8
    c-string
    file)
  (import (scheme) (syntax) (syntaxes) (lets) (port))

  (define-rules-syntaxes
    ((define-put-syntax $id $expr)
      (identifier? #'$id)
      (begin
        (define-keyword $id)
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
    (with-bytevector-output-port $port
      (put $port $op ...)))

  ; --- library ---

  (define-put-syntax (u8 $port $syntax)
    (syntax-case $syntax ()
      ((_ $expr)
        #`(put-u8 $port $expr))))

  (define-put-syntax (u16-le $port $syntax)
    (syntax-case $syntax ()
      ((_ $expr)
        #`(let (($u16 $expr))
          (put-u8 $port (fxand $u16 #xff))
          (put-u8 $port (fxsrl $u16 8))))))

  (define-put-syntax (u16-be $port $syntax)
    (syntax-case $syntax ()
      ((_ $expr)
        #`(let (($u16 $expr))
          (put-u8 $port (fxsrl $u16 8))
          (put-u8 $port (fxand $u16 #xff))))))

  (define-put-syntax (u32-le $port $syntax)
    (syntax-case $syntax ()
      ((_ $expr)
        #`(let (($u32 $expr))
          (put-u16 $port (fxand $u32 #xffff) 'little)
          (put-u16 $port (fxsrl $u32 16) 'little)))))

  (define-put-syntax (u32-be $port $syntax)
    (syntax-case $syntax ()
      ((_ $expr)
        #`(let (($u32 $expr))
          (put-u16 $port (fxsrl $u32 16) 'big)
          (put-u16 $port (fxand $u32 #xffff) 'big)))))

  (define-put-syntax (u64-le $port $syntax)
    (syntax-case $syntax ()
      ((_ $expr)
        #`(let (($u64 $expr))
          (put-u32 $port (bitwise-and $u64 #xffffffff) 'little)
          (put-u32 $port (bitwise-arithmetic-shift-right $u64 32) 'little)))))

  (define-put-syntax (u64-be $port $syntax)
    (syntax-case $syntax ()
      ((_ $expr)
        #`(let (($u64 $expr))
          (put-u32 $port (bitwise-arithmetic-shift-right $u64 32) 'big)
          (put-u32 $port (bitwise-and $u64 #xffffffff) 'big)))))

  (define-put-syntax (utf8 $port $syntax)
    (syntax-case $syntax ()
      ((_ $expr)
        #`(put-bytevector $port (string->utf8 $expr)))))

  (define-put (c-string $expr)
    (utf8 $expr)
    (u8 0))

  (define-put-syntax (file $port $syntax $lookup)
    (syntax-case $syntax ()
      ((_ $path)
        #`(call-with-port (open-file-input-port $path)
          (lambda ($input)
            (put-bytevector $port (get-bytevector-all $input)))))))
)
