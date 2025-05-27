(library (labs stacky)
  (export
    out inc
    db dw dz label
    call ret nop
    transform-vectorize
    transform-resolve
    transform-z80
    transform-flatten
    transform-assemble)
  (import (micascheme))

  (define-keyword out)
  (define-keyword inc)
  (define-keyword db)
  (define-keyword dw)
  (define-keyword dz)
  (define-keyword label)

  (define-keyword nop)
  (define-keyword call)
  (define-keyword ret)

  (define (transform-put-op $port $op)
    (syntax-case $op (db dw)
      ((db $expr)
        #`(put-u8 #,$port $expr))
      ((dw $expr)
        #`(put-u16 #,$port $expr (endianness little)))))

  (define (transform-vectorize $op-list)
    (with-implicit (transform-assemble $port)
      #`(let-values
        ((($port $done) (open-bytevector-output-port)))
        #,@(map (partial transform-put-op #'$port) $op-list)
        (bytevector->immutable-bytevector ($done)))))

  (define (transform-resolve $ops)
    (with-implicit (transform-assemble $port)
      (let ()
        (define $out-vars (stack))
        (define $out-ops (stack))
        (define $pc 0)
        (for-each
          (lambda ($op)
            (syntax-case $op (label db dw)
              ((label $name)
                (set! $out-vars (push $out-vars #`($name #,$pc))))
              ((db $expr)
                (let ()
                  (set! $out-ops (push $out-ops $op))
                  (set! $pc (+ $pc 1))))
              ((dw $expr)
                (let ()
                  (set! $out-ops (push $out-ops $op))
                  (set! $pc (+ $pc 2))))))
          $ops)
        (values
          (reverse $out-ops)
          (reverse $out-vars)))))

  (define (transform-z80 $ops)
    (map
      (lambda ($op)
        (syntax-case $op (nop call ret db dw dz)
          ((nop)
            #'(db #x00))
          ((call $expr)
            #'(begin
              (db #xcd)
              (dw $expr)))
          ((ret)
            #'(db #xc9))
          ((db $expr ...)
            #`(begin
              #,@(map
                (lambda ($expr) #`(db #,$expr))
                (syntax->list #'($expr ...)))))
          ((dw $expr ...)
            #`(begin
              #,@(map
                (lambda ($expr) #`(dw #,$expr))
                (syntax->list #'($expr ...)))))
          ((dz $expr ...)
            #`(begin
              #,@(map
                (lambda ($expr) #`(db #,$expr))
                (syntax->list #'($expr ...)))
              (db 0)))
          ($other #'$other)))
      $ops))

  (define (transform-flatten $ops)
    (flatten
      (map
        (lambda ($op)
          (syntax-case $op (begin)
            ((begin $expr ...)
              (transform-flatten (syntax->list #'($expr ...))))
            ($other
              (list #'$other))))
        $ops)))

  (define (transform-assemble $ops)
    (lets
      ($ops (transform-flatten $ops))
      ($ops (transform-z80 $ops))
      ($ops (transform-flatten $ops))
      ((values $ops $labels) (transform-resolve $ops))
      #`(lets
        #,@$labels
        #,(transform-vectorize $ops))))
)
