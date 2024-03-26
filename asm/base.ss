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

  (define-rule-syntax (define-asm-core-syntax-rule ($name $param ...) $body)
    (define-asm-core-syntax $name
      (lambda ($syntax $emit-u8 $org)
        (syntax-case $syntax ()
          ((_ $param ...) #`$body)))))


  (define-rule-syntax (define-asm-syntax-rule ($name $param ...) $body)
    (define-asm-syntax $name
      (lambda ($syntax)
        (syntax-case $syntax ()
          ((_ $param ...) #`$body)))))

  (define (asm-bytevector $asm)
    (lets
      ((values $port $close) (open-bytevector-output-port))
      ($emit-u8 (lambda ($u8) (put-u8 $port $u8)))
      (run ($asm $emit-u8))
      ($close)))
)
