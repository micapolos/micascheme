(import
  (except (micascheme) load)
  (masm model)
  (masm program))

(check
  (equal?
    (compiles '$mem (vector '$a '$b)
      (op (const (type (u8)) 10))
      (op (get (type (u8)) 0))
      (op (get (type (u8)) 1))
      (op (add (type (u8))))
      (op (out (type (u8))))
      (op (inc (type (u8))))
      (op (set (type (u8)) 0))
      (op (const (type (u16)) #x1000))
      (op (const (type (u8)) 10))
      (op (store (type (u8)))))
    '(lambda ($mem)
      (displayln (u8+ $a $b))
      (set! $a (u8+1 10))
      (bytevector-u8-set! $mem 4096 10)
      (void))))
