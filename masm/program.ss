(library (masm program)
  (export
    program? program program-exprs program-instrs
    program+op
    ops->instrs)
  (import
    (micascheme)
    (masm model))

  (data (program exprs instrs))

  (define (ops->instrs $ops)
    (reverse
      (program-instrs
        (fold-left
          program+op
          (program (stack) (stack))
          $ops))))

  (define (program+op $program $op)
    (switch-exclusive $op
      ((const-op? $const-op)
        (program
          (push
            (program-exprs $program)
            (const-op-u8 $const-op))
          (program-instrs $program)))
      ((inc-op? $inc-op)
        (program
          (lets
            ((pair $expr $exprs) (program-exprs $program))
            (push $exprs `(add1 ,$expr)))
          (program-instrs $program)))
      ((add-op? $add-op)
        (lets
          ((pair $rhs (pair $lhs $exprs)) (program-exprs $program))
          (program
            (push $exprs `(+ ,$lhs ,$rhs))
            (program-instrs $program))))
      ((get-op? $get-op)
        (program
          (push (program-exprs $program) (get-op-id $get-op))
          (program-instrs $program)))
      ((set-op? $set-op)
        (lets
          ((pair $expr $exprs) (program-exprs $program))
          (program
            $exprs
            (push
              (program-instrs $program)
              `(set! ,(set-op-id $set-op) ,$expr)))))
      ((load-op? $load-op)
        (lets
          ((pair $addr $exprs) (program-exprs $program))
          (program
            (push $exprs `(bytevector-u8-ref $mem ,$addr))
            (program-instrs $program))))
      ((store-op? $store-op)
        (lets
          ($exprs (program-exprs $program))
          ((pair $value $exprs) $exprs)
          ((pair $addr $exprs) $exprs)
          (program
            $exprs
            (push
              (program-instrs $program)
              `(bytevector-u8-set! $mem ,$addr ,$value)))))
      ((out-op? $out-op)
        (lets
          ((pair $expr $exprs) (program-exprs $program))
          (program
            $exprs
            (push
              (program-instrs $program)
              `(displayln ,$expr)))))))
)
