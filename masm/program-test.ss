(import
  (micascheme)
  (masm model)
  (masm program))

(check
  (equal?
    (compile-ops '$mem (vector '$a '$b)
      (const-op 10)
      (get-op 0)
      (get-op 1)
      (add-op)
      (out-op)
      (inc-op)
      (set-op 0)
      (const-op #x1000)
      (const-op 10)
      (store-op))
    '(lambda ($mem)
      (displayln (+ $a $b))
      (set! $a (add1 10))
      (bytevector-u8-set! $mem 4096 10)
      (void))))
