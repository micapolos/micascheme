(library (masm program)
  (export
    program? program program-exprs program-instrs
    program+op
    compiles)
  (import
    (except (micascheme) load)
    (masm model))

  (data (program exprs instrs))

  (define (compiles $mem $locals . $ops)
    `(lambda (,$mem)
      ,@(reverse
        (program-instrs
          (fold-left
            (partial program+op $mem $locals)
            (program (stack) (stack))
            $ops)))
      (void)))

  (define (program+op $mem $locals $program $op)
    (op-switch $op
      ((const? $const)
        (program
          (push
            (program-exprs $program)
            (const-u8 $const))
          (program-instrs $program)))
      ((inc? $inc)
        (program
          (lets
            ((pair $expr $exprs) (program-exprs $program))
            (push $exprs `(add1 ,$expr)))
          (program-instrs $program)))
      ((add? $add)
        (lets
          ((pair $rhs (pair $lhs $exprs)) (program-exprs $program))
          (program
            (push $exprs `(+ ,$lhs ,$rhs))
            (program-instrs $program))))
      ((get? $get)
        (program
          (push
            (program-exprs $program)
            (vector-ref $locals (get-idx $get)))
          (program-instrs $program)))
      ((set? $set)
        (lets
          ((pair $expr $exprs) (program-exprs $program))
          (program
            $exprs
            (push
              (program-instrs $program)
              `(set! ,(vector-ref $locals (set-idx $set)) ,$expr)))))
      ((load? $load)
        (lets
          ((pair $addr $exprs) (program-exprs $program))
          (program
            (push $exprs `(bytevector-u8-ref ,$mem ,$addr))
            (program-instrs $program))))
      ((store? $store)
        (lets
          ($exprs (program-exprs $program))
          ((pair $value $exprs) $exprs)
          ((pair $addr $exprs) $exprs)
          (program
            $exprs
            (push
              (program-instrs $program)
              `(bytevector-u8-set! ,$mem ,$addr ,$value)))))
      ((out? $out)
        (lets
          ((pair $expr $exprs) (program-exprs $program))
          (program
            $exprs
            (push
              (program-instrs $program)
              `(displayln ,$expr)))))))
)
