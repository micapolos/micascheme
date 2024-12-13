(library (zexy vectorize)
  (export
    vectorize)
  (import
    (micascheme)
    (labs syntax)
    (only (zexy ops) db dw ds))

  (define (vectorize-op $op)
    #`(lambda ($put-u8)
      #,(syntax-case $op (db dw ds)
        ((db $expr)
          #`($put-u8 $expr))
        ((dw $expr)
          #`(lets
            ($value $expr)
              (run
                ($put-u8 (fxand $value #xff))
                ($put-u8 (fxsrl $value 8)))))
        ((ds $expr)
          #`(repeat $expr ($put-u8 0))))))

  (define (vectorize $ops)
    #`(with-bytevector-output-port $port
      (for-each
        (lambda ($fn) ($fn (lambda ($u8) (put-u8 $port $u8))))
        (list #,@(flat-map-syntax-list vectorize-op $ops)))))
)
