(library (zexy vectorize)
  (export
    vectorize)
  (import
    (micascheme)
    (labs syntax)
    (only (zexy ops) db dw ds))

  (define (vectorize $ops)
    #`(lets
      ((values $port $close) (open-bytevector-output-port))
      (run
        #,@(flat-map-syntax-list
          (lambda ($op)
            (syntax-case $op (db dw ds)
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
