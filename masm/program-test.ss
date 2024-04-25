(import
  (except (micascheme) load)
  (masm model)
  (masm program))

(check
  (equal?
    (compiles '$mem (vector '$a '$b)
      (op (const 10))
      (op (get 0))
      (op (get 1))
      (op (add))
      (op (out))
      (op (inc))
      (op (set 0))
      (op (const #x1000))
      (op (const 10))
      (op (store)))
    '(lambda ($mem)
      (displayln (+ $a $b))
      (set! $a (add1 10))
      (bytevector-u8-set! $mem 4096 10)
      (void))))
