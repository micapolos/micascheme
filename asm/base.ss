(library (asm base)
  (export
    org db-proc db dw label
    asm-bytevector)
  (import (micascheme))

  (define org
    (make-thread-parameter 0))

  (define db-proc
    (make-thread-parameter
      (lets
        ($port (standard-output-port))
        (lambda ($u8) (put-u8 $port $u8)))))

  (define-syntax-rule (db $u8)
    (begin
      ((db-proc) $u8)
      (org (add1 (org)))))

  (define-syntax-rule (dw $u16)
    (begin
      (lets
        ($val $u16)
        (db (fxand $val #xff))
        (db (fxsrl $val 8)))))

  (define-syntax-rule (label $name)
    (define $name (org)))

  (define-syntax-rule (asm-bytevector $body ...)
    (lets
      ((values $port $close) (open-bytevector-output-port))
      ($db-proc (lambda ($u8) (put-u8 $port $u8)))
      (run
        (parameterize ((org 0) (db-proc $db-proc)) $body ...)
        ($close))))
)
