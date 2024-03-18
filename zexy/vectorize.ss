(library (zexy vectorize)
  (export
    vectorize)
  (import
    (micascheme)
    (only (zexy ops) db dw ds))

  (define (vectorize $ops)
    #`(lets
      ((values $port $close) (open-bytevector-output-port))
      (run
        #,@(map
          (lambda ($op)
            (syntax-case $op (db dw)
              ((db $expr)
                #`(put-u8 $port $expr))
              ((dw $expr)
                #`(lets
                  ($value $expr)
                  (run
                    (put-u8 $port (fxand $value #xff))
                    (put-u8 $port (fxsrl $value 8)))))
              ((ds $expr)
                #`(repeat $expr (put-u8 $port 0)))))
          $ops)
        ($close))))
)
