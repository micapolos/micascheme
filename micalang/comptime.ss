(library (micalang comptime)
  (export
    literal app let
    type bool int
    inc dec = + - < zero?
    list
    pi)
  (import
    (except (micalang base) = + - < zero? list app lambda let)
    (prefix (only (micalang base) lambda let) %)
    (rename (micalang term) (pi %pi)))
  (export
    (import
      (only (micascheme) lambda equal?)
      (only (micalang term) native variable application pi-param)))

  (define-rule-syntax (literal x)
    (native x))

  (define-rule-syntax (let (id x) body)
    (%let ((id x)) body))

  (define-rules-syntax
    ((lambda id body)
      (abstraction (%lambda (id) body))))

  (define-rules-syntax
    ((define-prim id prim)
      (define id (native prim)))
    ((define-prim id x prim)
      (define id
        (lambda x
          (switch x
            ((native? $native) (native (prim (native-ref $native))))
            ((else $other) (application (native id) $other))))))
    ((define-prim id x y prim)
      (define id
        (lambda x
          (lambda y
            (switch x
              ((native? $native-x)
                (switch y
                  ((native? $native-y)
                    (native
                      (prim
                        (native-ref $native-x)
                        (native-ref $native-y))))
                  ((else $other-y)
                    (application (application (native id) $native-x) $other-y))))
              ((else $other-x)
                (application (application (native id) $other-x) y))))))))

  (define-rule-syntax (define-prims (id arg ... prim) ...)
    (begin (define-prim id arg ... prim) ...))

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

  (define-prims
    (type 'type)
    (bool 'bool)
    (int 'int)

    (zero? x fxzero?)
    (inc x fx+1/wraparound)
    (dec x fx-1/wraparound)

    (= x y fx=)
    (+ x y fx+/wraparound)
    (- x y fx-/wraparound)
    (< x y fx<))

  (define list
    (lambda x (application (native list) x)))

  (define-rules-syntax
    ((pi (id in) out)
      (%pi in (%lambda (id) out)))
    ((pi in out)
      (pi (_ in) out)))
)
