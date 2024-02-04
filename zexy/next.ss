(library (zexy next)
  (export
    make-next next?
    next-z80
    next-mmus
    next-banks
    next-string

    next-run

    next-mmu next-mmu!

    next-bank
    next-rd next-wr
    next-in next-out

    next-print-char
    next-print
    next-println)
  (import
    (zexy base)
    (zexy z80))


  (define-record next () (
    ((immutable z80) (make-z80))
    ((immutable mmus) (make-bytevector 8 0))
    ((immutable banks) (build-immutable-vector #x100 make-bank))
    ((mutable char-stack) (stack))))

  (define (make-bank $index)
    (make-bytevector #x2000 0))

  (define (next-bank $next $index)
    (vector-ref (next-banks $next) $index))

  (define (next-mmu $next $slot)
    (bytevector-u8-ref (next-mmus $next) $slot))

  (define (next-mmu! $next $slot $bank)
    (bytevector-u8-set! (next-mmus $next) $slot $bank))

  (define (next-rd $next $addr)
    (bytevector-u8-ref
      (vector-ref
        (next-banks $next)
        (bytevector-u8-ref
          (next-mmus $next)
          (shr $addr 13)))
      (band $addr #x1fff)))

  (define (next-wr $next $addr $u8)
    (bytevector-u8-set!
      (vector-ref
        (next-banks $next)
        (bytevector-u8-ref
          (next-mmus $next)
          (shr $addr 13)))
      (band $addr #x1fff)
      $u8))

  (define (next-in $next $addr)
    0)

  (define (next-out $next $addr $u8)
    (case $addr
      ; I2C SDA line
      ((#x113B)
        (set-next-char-stack! $next
          (push (next-char-stack $next) (integer->char $u8))))
      (else (void))))

  (define (next-print-char $next $char)
    (next-out $next #x113b (band (char->integer $char) #xff)))

  (define (next-print $next $string)
    (string-for-each (partial next-print-char $next) $string))

  (define (next-println $next $string)
    (next-print $next $string)
    (next-print-char $next #\newline))

  (define (next-run $next)
    (z80-run (next-z80 $next)
      (partial next-rd $next)
      (partial next-wr $next)
      (partial next-in $next)
      (partial next-out $next)))

  (define (next-string $next)
    (list->string (reverse (next-char-stack $next))))
)
