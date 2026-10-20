(library (arm va64)
  (export
    fn fn?
    fn-arity
    fn-ret)
  (import
    (scheme)
    (fixnum))

  (define op-shift 26)

  (define op-fn #x01)

  (define (fn arity ret)
    (fxpack
      (6 op-fn)
      (3 arity)
      (1 ret)
      (22 0)))

  (define (fn? $op)
    (fx= (fxsrl $op op-shift) op-fn))

  (define (fn-arity $fn)
    (fxand (fxsrl $fn 23) #b111))

  (define (fn-ret $fn)
    (fxand (fxsrl $fn 22) #b1))
)
