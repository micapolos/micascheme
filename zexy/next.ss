(library (zexy next)
  (export
    make-next next? next-mmus next-banks
    next-rd next-wr
    next-in next-out

    next-print-char
    next-print
    next-println)
  (import
    (zexy base))

  (define-record next () (
    ((immutable mmus) (make-bytevector 8))
    ((immutable banks) (build-immutable-vector #x100 make-bank))))

  (define (make-bank $index)
    (make-bytevector #x2000))

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
      ((#x113B) (display (integer->char $u8)))
      (else (void))))

  (define (next-print-char $next $char)
    (next-out $next #x113b (band (char->integer $char) #xff)))

  (define (next-print $next $string)
    (string-for-each (partial next-print-char $next) $string))

  (define (next-println $next $string)
    (next-print $next $string)
    (next-print-char $next #\newline))
)
