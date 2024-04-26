(import
  (except (micascheme) module)
  (masm model)
  (masm program))

(check
  (equal?
    (compile-ops (list '$a '$b) (stack)
      (list
        (op (const (int (i8)) 10))
        (op (const (int (i16)) 8912))
        (op (local-get (type (int (i8))) 0))
        (op (local-get (type (int (i8))) 1))
        (op (add (int (i8))))
        (op (io-set))
        (op (inc (int (i8))))
        (op (local-set (type (int (i8))) 0))
        (op (const (int (i16)) 4096))
        (op (const (int (i8)) 10))
        (op (mem-set (int (i8))))))
    '(
      (io-set 8912 (i8+ $a $b))
      (set! $a (i8+1 10))
      (mem-i8-set! $mem 4096 10))))

(check
  (equal?
    (compile-func
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
          (op (const (int (i16)) 8912))
          (op (local-get (type (int (i8))) 0))
          (op (io-set)))))
    '(lambda ($0 $1)
      (define $2)
      (set! $2 (i8+ $0 $1))
      (io-set 8912 $2)
      (values $2))))

(check
  (equal?
    (compile-module
      (module
        (list
          (func
            (arrow
              (list (type (int (i8))))
              (list (type (int (i8)))))
            (list)
            (list))
          (func
            (arrow
              (list (type (int (i8))) (type (int (i8))))
              (list (type (int (i8)))))
            (list)
            (list (op (add (int (i8)))))))))
    '(
      (define $f0 (lambda ($0) (values $0)))
      (define $f1 (lambda ($0 $1) (values (i8+ $0 $1)))))))
