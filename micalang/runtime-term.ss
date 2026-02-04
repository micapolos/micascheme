(library (micalang runtime-term)
  (export
    type bool int
    inc dec + - < zero?
    list
    pi pi-param)
  (import
    (except (micalang base) + - < zero? list)
    (rename (micalang term) (pi %pi) (pi-param %%pi-param)))
  (export
    (import
      (only (micascheme) lambda equal?)
      (only (micalang term) native variable application)))

  ; === selectors passed to procedures:
  (data %pi?)       ; #t for pi, #f for lambda
  (data %pi-param)  ; pi param

  (define type (native 'type))
  (define bool (native 'bool))
  (define int (native 'int))

  (define zero?
    (lambda (x)
      (switch x
        ((fixnum? $fixnum) (fxzero? $fixnum))
        ((%pi?? _) #f)
        ((else $other) (application (native zero?) $other)))))

  (define inc
    (lambda (x)
      (switch x
        ((fixnum? $fixnum) (fx+/wraparound $fixnum 1))
        ((%pi?? _) #f)
        ((else $other) (application (native inc) $other)))))

  (define dec
    (lambda (x)
      (switch x
        ((fixnum? $fixnum) (fx-/wraparound $fixnum 1))
        ((%pi?? _) #f)
        ((else $other) (application (native dec) $other)))))

  (define +
    (lambda (x)
      (lambda (y)
        (switch x
          ((fixnum? $fixnum-x)
            (switch y
              ((fixnum? $fixnum-y)
                (fx+/wraparound $fixnum-x $fixnum-y))
              ((%pi?? _) #f)
              ((else $other-y)
                (application (application (native +) $fixnum-x) $other-y))))
          ((%pi?? _) #f)
          ((else $other-x)
            (application (application (native +) $other-x) y))))))

  (define -
    (lambda (x)
      (lambda (y)
        (switch x
          ((fixnum? $fixnum-x)
            (switch y
              ((fixnum? $fixnum-y)
                (fx-/wraparound $fixnum-x $fixnum-y))
              ((%pi?? _) #f)
              ((else $other-y)
                (application (application (native -) $fixnum-x) $other-y))))
          ((%pi?? _) #f)
          ((else $other-x)
            (application (application (native -) $other-x) y))))))

  (define <
    (lambda (x)
      (lambda (y)
        (switch x
          ((fixnum? $fixnum-x)
            (switch y
              ((fixnum? $fixnum-y)
                (fx< $fixnum-x $fixnum-y))
              ((%pi?? _) #f)
              ((else $other-y)
                (application (application (native <) $fixnum-x) $other-y))))
          ((%pi?? _) #f)
          ((else $other-x)
            (application (application (native <) $other-x) y))))))

  (define list
    (lambda (x)
      (switch x
        ((%pi?? _) #f)
        ((else _) (application (native list) x)))))

  (define pi-param
    (lambda ($pi)
      ($pi %pi-param)))

  (define-rules-syntax
    ((pi (id in) out)
      (lambda (id)
        (switch id
          ((%pi?? _) #t)
          ((%pi-param? _) in)
          ((else _) out)))))
)
