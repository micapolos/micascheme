(import
  (except (micascheme) load module)
  (masm model)
  (masm program))

(check
  (equal?
    (compile-ops '$mem (list '$a '$b) (stack)
      (list
        (op (const (int (i8)) 10))
        (op (local-get (type (int (i8))) 0))
        (op (local-get (type (int (i8))) 1))
        (op (add (int (i8))))
        (op (out (int (i8))))
        (op (inc (int (i8))))
        (op (local-set (type (int (i8))) 0))
        (op (const (int (i16)) #x1000))
        (op (const (int (i8)) 10))
        (op (mem-set (int (i8))))))
    '(
      (displayln (i8+ $a $b))
      (set! $a (i8+1 10))
      (mem-i8-set! $mem 4096 10))))

(check
  (equal?
    (compile-func
      '$mem
      (func
        (arrow
          (list
            (type (int (i8)))
            (type (int (i8))))
          (list (type (int (i8)))))
        (list
          (type (int (i8))))
        (list
          (op (add (int (i8))))
          (op (local-set (type (int (i8))) 0))
          (op (local-get (type (int (i8))) 0))
          (op (local-get (type (int (i8))) 0))
          (op (out (int i8))))))
    '(lambda ($i8-0 $i8-1)
      (define $i8-2)
      (set! $i8-2 (i8+ $i8-0 $i8-1))
      (displayln $i8-2)
      (values $i8-2))))
