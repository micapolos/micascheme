(library (procek procek)
  (export
    make-zx
    zx-run
    zx-regs
    regs-pc regs-pc-set!

    jp stop display-pc)
  (import (micascheme))

  (define-record-type zx
    (fields regs banks slots)
    (protocol
      (lambda (new)
        (lambda ()
          (new (make-regs) (make-banks) (make-slots))))))

  (define (make-regs)
    (make-bytevector 16 0))

  (define (make-bank)
    (make-bytevector 8192 0))

  (define (make-banks)
    (build-vector 192
      (lambda ($index)
        (make-bank))))

  (define (make-slots)
    (make-bytevector 8 0))

  (define-rule-syntax (regs-pc $regs)
    (bytevector-u16-ref $regs 0 (endianness little)))

  (define-rule-syntax (regs-pc-set! $regs $u16)
    (bytevector-u16-set! $regs 0 $u16 (endianness little)))

  (define (zx-run $zx $code)
    (lets
      ($regs (zx-regs $zx))
      (do ()
        ((lets
          ($pc (regs-pc $regs))
          ($proc (vector-ref $code $pc))
          (run (regs-pc-set! $regs (fx+/wraparound $pc 1)))
          ($proc $zx)) (void)))))

  (define-rule-syntax (stop)
    (lambda ($zx) #t))

  (define-rule-syntax (define-zx-ops (zx) (pattern body ...) ...)
    (define-rules-syntaxes
      (pattern (lambda (zx) body ... #f)) ...))

  (define-zx-ops ($zx)
    ((jp $pc)
      (regs-pc-set! (zx-regs $zx) $pc))
    ((display-pc)
      (display "PC: ")
      (display (regs-pc (zx-regs $zx)))
      (newline)))
)
