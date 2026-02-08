(library (micalang comptime)
  (export
    prim curry
    literal app lambda let if
    type boolean number symbol char string
    = + - < zero?
    pi)
  (import
    (except (micalang base) = + - < zero? list app lambda let string if)
    (prefix (only (micalang base) lambda let if = + - < zero?) %)
    (rename (micalang term) (pi %pi)))
  (export
    (import
      (only (micascheme) equal? quote)
      (only (micalang term) native application pi-param)))

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
      (lambda x (term-apply (native ($primitive 3 id)) x)))
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

  (define array
    (lambda n (application (native array) n)))

  (define-prims
    (type    'type)
    (boolean 'boolean)
    (number  'number)
    (symbol  'symbol)
    (char    'char)
    (string  'string)

    (zero? x %zero?)

    (= x y %=)
    (+ x y %+)
    (- x y %-)
    (< x y %<))

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
