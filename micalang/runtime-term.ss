(library (micalang runtime-term)
  (export
    literal app
    type bool int
    inc dec = + - < zero?
    list
    pi)
  (import
    (except (micalang base) = + - < zero? list app)
    (rename (micalang term) (pi %pi)))
  (export
    (import
      (only (micascheme) lambda equal?)
      (only (micalang term) native variable application pi-param)))

  (define-rule-syntax (literal x)
    (native x))

  (define (app $lhs $rhs)
    (switch $lhs
      ((pi? $pi)
        ((pi-procedure $pi) $rhs))
      ((abstraction? $abstraction)
        ((abstraction-procedure $abstraction) $rhs))
      ((else $other)
        (application (native $other) $rhs))))

  ; === selectors passed to procedures:
  (data %pi?)       ; #t for pi, #f for lambda
  (data %pi-param)  ; pi param

  (define type (native 'type))
  (define bool (native 'bool))
  (define int (native 'int))

  (define zero?
    (abstraction
      (lambda (x)
        (switch x
          ((native? $native) (native (fxzero? (native-ref $native))))
          ((else $other) (application (native zero?) $other))))))

  (define inc
    (abstraction
      (lambda (x)
        (switch x
          ((native? $native) (native (fx+/wraparound (native-ref $native) 1)))
          ((else $other) (application (native inc) $other))))))

  (define dec
    (abstraction
      (lambda (x)
        (switch x
          ((native? $native) (native (fx-/wraparound (native-ref $native) 1)))
          ((else $other) (application (native dec) $other))))))

  (define =
    (abstraction
      (lambda (x)
        (abstraction
          (lambda (y)
            (switch x
              ((native? $native-x)
                (switch y
                  ((native? $native-y)
                    (native
                      (fx=
                        (native-ref $native-x)
                        (native-ref $native-y))))
                  ((else $other-y)
                    (application (application (native =) $native-x) $other-y))))
              ((else $other-x)
                (application (application (native =) $other-x) y))))))))

  (define +
    (abstraction
      (lambda (x)
        (abstraction
          (lambda (y)
            (switch x
              ((native? $native-x)
                (switch y
                  ((native? $native-y)
                    (native
                      (fx+/wraparound
                        (native-ref $native-x)
                        (native-ref $native-y))))
                  ((else $other-y)
                    (application (application (native +) $native-x) $other-y))))
              ((else $other-x)
                (application (application (native +) $other-x) y))))))))

  (define -
    (abstraction
      (lambda (x)
        (abstraction
          (lambda (y)
            (switch x
              ((native? $native-x)
                (switch y
                  ((native? $native-y)
                    (native
                      (fx-/wraparound
                        (native-ref $native-x)
                        (native-ref $native-y))))
                  ((else $other-y)
                    (application (application (native -) $native-x) $other-y))))
              ((else $other-x)
                (application (application (native -) $other-x) y))))))))

  (define <
    (abstraction
      (lambda (x)
        (abstraction
          (lambda (y)
            (switch x
              ((native? $native-x)
                (switch y
                  ((native? $native-y)
                    (native
                      (fx<
                        (native-ref $native-x)
                        (native-ref $native-y))))
                  ((else $other-y)
                    (application (application (native <) $native-x) $other-y))))
              ((else $other-x)
                (application (application (native <) $other-x) y))))))))

  (define list
    (abstraction
      (lambda (x)
        (application (native list) x))))

  (define-rules-syntax
    ((pi (id in) out)
      (%pi in (lambda (id) out)))
    ((pi in out)
      (pi (_ in) out)))
)
