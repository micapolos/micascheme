(library (asm data)
  (export data db dw data->datum check-data)
  (import (except (micascheme) data) (u))

  (define data binary-append)
  (define (db . $xs)
    (apply binary-append
      (map-with ($x $xs)
        (switch $x
          ((u8? $u8) (u8-binary $u8))
          ((char? $char) (u8-binary (char->ascii $char)))
          ((string? $string) (bytevector-binary (string->ascii $string)))))))
  (define (dw . $x) (apply data (map (partial-flip u16-binary 'little) $x)))

  (define (data->datum . $xs)
    `(db ,@(bytevector->u8-list (binary->bytevector (apply data $xs)))))

  (define-rule-syntax (check-data in ... out)
    (parameterize ((print-radix 16)) (check (equal? (data->datum in ...) 'out))))
)
