(library (micalang runtime-term)
  (export bool int inc dec + - < zero?)
  (import
    (except (micalang base) + - < zero?)
    (micalang term))
  (export
    (import
      (only (micascheme) lambda equal?)
      (micalang term)))

  (define bool (native 'bool))
  (define int (native 'int))

  (define zero?
    (lambda (x)
      (switch x
        ((fixnum? $fixnum) (fxzero? $fixnum))
        ((else $other) (application (native zero?) $other)))))

  (define inc
    (lambda (x)
      (switch x
        ((fixnum? $fixnum) (fx+/wraparound $fixnum 1))
        ((else $other) (application (native inc) $other)))))

  (define dec
    (lambda (x)
      (switch x
        ((fixnum? $fixnum) (fx-/wraparound $fixnum 1))
        ((else $other) (application (native dec) $other)))))

  (define +
    (lambda (x)
      (lambda (y)
        (switch x
          ((fixnum? $fixnum-x)
            (switch y
              ((fixnum? $fixnum-y)
                (fx+/wraparound $fixnum-x $fixnum-y))
              ((else $other-y)
                (application (application (native +) $fixnum-x) $other-y))))
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
              ((else $other-y)
                (application (application (native -) $fixnum-x) $other-y))))
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
              ((else $other-y)
                (application (application (native <) $fixnum-x) $other-y))))
          ((else $other-x)
            (application (application (native <) $other-x) y))))))
)
