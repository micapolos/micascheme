(import
  (micascheme)
  (masm model)
  (masm program))

(check
  (equal?
    (ops->instrs
      (list
        (const-op 10)
        (get-op '$a)
        (get-op '$b)
        (add-op)
        (out-op)
        (inc-op)
        (set-op '$out)
        (const-op #x1000)
        (const-op 10)
        (store-op)))
    '(
      (displayln (+ $a $b))
      (set! $out (add1 10))
      (bytevector-u8-set! $mem #x1000 10))))
