(library (masm program)
  (export
    program? program program-exprs program-instrs
    program+op
    compile-module
    compile-func
    compile-ops)
  (import
    (except (micascheme) module)
    (masm model))

  (data (program exprs instrs))

  (define (compile-ops $funcs $locals $exprs $ops)
    (reverse
      (program-instrs
        (fold-left
          (partial program+op $funcs $locals)
          (program $exprs (stack))
          $ops))))

  (define (sym $index $type)
    (string->symbol
      (string-append "$" (number->string $index))))

  (define (compile-module (module $funcs))
    (lets
      ($sym-arrows
        (map-indexed
          (lambda ($index $func)
            (cons
              (string->symbol
                (string-append "$f" (number->string $index)))
              (func-arrow $func)))
          $funcs))
      ($lambdas
        (map compile-func $sym-arrows $funcs))
      (map
        (lambda ($sym $lambda)
          `(define ,$sym ,$lambda))
        (map car $sym-arrows)
        $lambdas)))

  (define (compile-func $sym-arrows $func)
    (lets
      ($arrow (func-arrow $func))
      ($ins (arrow-ins $arrow))
      ($outs (arrow-outs $arrow))
      ($param-count (length $ins))
      ($locals (map-indexed
        (lambda ($index $type)
          (sym (+ $index $param-count) $type))
        (func-locals $func)))
      ($defines
        (map
          (lambda ($local)
            `(define ,$local))
          $locals))
      ($params (map-indexed sym $ins))
      ((program $exprs $instrs)
        (fold-left
          (partial program+op $sym-arrows $locals)
          (program (reverse $params) (stack))
          (func-ops $func)))
      `(lambda (,@$params)
        ,@$defines
        ,@(reverse $instrs)
        (values ,@(reverse $exprs)))))

  (define (program+op $sym-arrows $locals $program $op)
    (op-switch $op
      ((drop? $drop)
        (program
          (cdr (program-exprs $program))
          (program-instrs $program)))
      ((select? $select)
        (lets
          ($exprs (program-exprs $program))
          ((pair $cond $exprs) $exprs)
          ((pair $false $exprs) $exprs)
          ((pair $true $exprs) $exprs)
          (program
            (push $exprs
              `(if (zero? ,$cond) ,$true ,$false))
            (program-instrs $program))))
      ((const? $const)
        (program
          (push
            (program-exprs $program)
            (const-value $const))
          (program-instrs $program)))
      ((inc? $inc)
        (program
          (lets
            ((pair $expr $exprs) (program-exprs $program))
            (push $exprs
              `(
                ,(int-switch (inc-int $inc)
                  ((i8? _) 'i8+1)
                  ((i16? _) 'i16+1))
                ,$expr)))
          (program-instrs $program)))
      ((add? $add)
        (lets
          ((pair $rhs (pair $lhs $exprs)) (program-exprs $program))
          (program
            (push $exprs
              `(
                ,(int-switch (add-int $add)
                  ((i8? _) 'i8+)
                  ((i16? _) 'i16+))
                ,$lhs
                ,$rhs))
            (program-instrs $program))))
      ((local-get? $local-get)
        (program
          (push
            (program-exprs $program)
            (list-ref $locals (local-get-idx $local-get)))
          (program-instrs $program)))
      ((local-set? $local-set)
        (lets
          ((pair $expr $exprs) (program-exprs $program))
          (program
            $exprs
            (push
              (program-instrs $program)
              `(set! ,(list-ref $locals (local-set-idx $local-set)) ,$expr)))))
      ((mem-get? $mem-get)
        (lets
          ((pair $addr $exprs) (program-exprs $program))
          (program
            (push $exprs
              `(
                ,(int-switch (mem-get-int $mem-get)
                  ((i8? _) 'mem-i8-ref)
                  ((i16? _) 'mem-i16-ref))
                $mem
                ,$addr))
            (program-instrs $program))))
      ((mem-set? $mem-set)
        (lets
          ($exprs (program-exprs $program))
          ((pair $value $exprs) $exprs)
          ((pair $addr $exprs) $exprs)
          (program
            $exprs
            (push
              (program-instrs $program)
              `(
                ,(int-switch (mem-set-int $mem-set)
                    ((i8? _) 'mem-i8-set!)
                    ((i16? _) 'mem-i16-set!))
                $mem
                ,$addr
                ,$value)))))
      ((io-get? $io-get)
        (lets
          ((pair $addr $exprs) (program-exprs $program))
          (program
            (push $exprs `(io-get ,$addr))
            (program-instrs $program))))
      ((io-set? $io-set)
        (lets
          ($exprs (program-exprs $program))
          ((pair $value $exprs) $exprs)
          ((pair $port $exprs) $exprs)
          (program
            $exprs
            (push
              (program-instrs $program)
              `(io-set ,$port ,$value)))))
      ((call? (call $idx))
        (lets
          ((pair $sym $arrow) (list-ref $sym-arrows $idx))
          ((values $args $exprs)
            (split
              (program-exprs $program)
              (length (arrow-ins $arrow))))
          ($expr `(,$sym ,@(reverse $args)))
          (case (length (arrow-outs $arrow))
            ((0)
              (program
                $exprs
                (push (program-instrs $program) $expr)))
            ((1)
              (program
                (push $exprs $expr)
                (program-instrs $program)))
            (else (throw many-outs)))))
      ((nop? $nop)
        $program)
      ((trap? $trap)
        (program
          (program-exprs $program)
          (push
            (program-instrs $program)
            '(trap))))
      ((block? $block)
        (fold-left
          (partial program+op $sym-arrows $locals)
          $program
          (block-ops $block)))))
)
