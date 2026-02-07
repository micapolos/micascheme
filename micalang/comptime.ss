(library (micalang comptime)
  (export
    prim
    literal app let if
    type bool int symbol string
    inc dec = + - < zero?
    index first-index last-index
    array
    list
    pi)
  (import
    (except (micalang base) = + - < zero? list app lambda let string if)
    (prefix (only (micalang base) lambda let if) %)
    (rename (micalang term) (pi %pi)))
  (export
    (import
      (only (micascheme) lambda equal? quote)
      (only (micalang term) native variable application pi-param)))

  (define-rule-syntax (literal x)
    (native x))

  (define-rule-syntax (let (id x) body)
    (%let ((id x)) body))

  (define-rules-syntax
    ((lambda id body)
      (abstraction 'id (%lambda (id) body))))

  (define-rules-syntax
    ((prim id)
      (native ($primitive 3 id)))
    ((prim id x)
      (lambda x
        (switch x
          ((native? $native) (native (($primitive 3 id) (native-ref $native))))
          ((else $other) (application (native ($primitive 3 id)) $other)))))
    ((prim id x y)
      (lambda x
        (lambda y
          (switch x
            ((native? $native-x)
              (switch y
                ((native? $native-y)
                  (native
                    (($primitive 3 id)
                      (native-ref $native-x)
                      (native-ref $native-y))))
                ((else $other-y)
                  (application (application (native ($primitive 3 id)) $native-x) $other-y))))
            ((else $other-x)
              (application (application (native ($primitive 3 id)) $other-x) y)))))))


  (define-rules-syntax
    ((curry p)
      (native p))
    ((curry x p)
      (lambda x
        (switch x
          ((native? $native) (native (p (native-ref $native))))
          ((else $other) (application (native p) $other)))))
    ((curry x y p)
      (lambda x
        (lambda y
          (switch x
            ((native? $native-x)
              (switch y
                ((native? $native-y)
                  (native
                    (p
                      (native-ref $native-x)
                      (native-ref $native-y))))
                ((else $other-y)
                  (application (application (native p) $native-x) $other-y))))
            ((else $other-x)
              (application (application (native p) $other-x) y)))))))

  (define-rules-syntax
    ((define-prim id arg ... p)
      (define id (curry arg ... p))))

  (define-rule-syntax (define-prims (id arg ... prim) ...)
    (begin (define-prim id arg ... prim) ...))

  (define (app $lhs $rhs)
    (switch $lhs
      ((pi? $pi)
        (pi-apply $pi $rhs))
      ((abstraction? $abstraction)
        (abstraction-apply $abstraction $rhs))
      ((else $other)
        (application (native $other) $rhs))))

  ; === selectors passed to procedures:
  (data %pi?)       ; #t for pi, #f for lambda
  (data %pi-param)  ; pi param

  (define (index n) (application index n))
  (define (%first-index n) 0)
  (define (%last-index   n) (fx-1/wraparound n))

  (define array
    (lambda n (application (native array) n)))

  (define-prims
    (type 'type)
    (bool 'bool)
    (int 'int)
    (symbol 'symbol)
    (string 'string)

    (zero? x fxzero?)
    (inc x fx+1/wraparound)
    (dec x fx-1/wraparound)

    (= x y fx=)
    (+ x y fx+/wraparound)
    (- x y fx-/wraparound)
    (< x y fx<)

    (first-index n %first-index)
    (last-index  n %last-index))

  (define list
    (lambda x (application (native list) x)))

  (define-rules-syntax
    ((pi (id in) out)
      (%pi 'id in (%lambda (id) out)))
    ((pi in out)
      (%pi #f in (%lambda (id) out))))

  (define-rule-syntax (if cond true false)
    (switch cond
      ((native? $native)
        (%if (native-ref $native) true false))
      ((else $other-cond)
        (conditional $other-cond true false))))

)
