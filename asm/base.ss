(library (asm base)
  (export
    define-asm-core-syntax-rule
    define-asm-syntax-rule
    asm-bytevector)

  (import
    (micascheme)
    (labs syntax)
    (asm core))

  (export
    (import (asm core)))

  (define-syntax-rule (define-asm-core-syntax-rule ($name $param ...) $body)
    (define-asm-core-syntax $name
      (lambda ($syntax $emit $org)
        (syntax-case $syntax ()
          ((_ $param ...) #`$body)))))


  (define-syntax-rule (define-asm-syntax-rule ($name $param ...) $body)
    (define-asm-syntax $name
      (lambda ($syntax)
        (syntax-case $syntax ()
          ((_ $param ...) #`$body)))))

  (define (asm-bytevector $asm)
    (lets
      ((values $port $close) (open-bytevector-output-port))
      ($emit (lambda ($u8) (put-u8 $port $u8)))
      (run ($asm $emit))
      ($close)))
)
