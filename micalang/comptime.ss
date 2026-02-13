(library (micalang comptime)
  (export
    curry constant tagged
    native app lambda macro let if
    a-boolean a-number a-symbol a-char a-string
    = + - < zero?
    string-append string-length number->string
    a-lambda)
  (import
    (except (micalang base) = + - < zero? list app lambda let string if string-append string-length number->string)
    (prefix (only (micalang base) lambda let if = + - < zero?) %)
    (prefix (micascheme) %%)
    (rename (micalang term) (type-abstraction %pi) (native %native) (constant %constant) (tagged %tagged) (macro %macro)))
  (export
    (import
      (only (micascheme) equal? quote quasiquote unquote syntax unsyntax quasisyntax ... datum syntax-case)
      (prefix (micascheme) %%)
      (only (micalang term) application type-abstraction-param a-type)
      (prefix (micalang term) %%)))

  (define-rule-syntax (native x)
    (%native x))

  (define-rule-syntax (constant x)
    (%constant 'x))

  (define-rule-syntax (tagged tag x)
    (%tagged tag x))

  (define-rule-syntax (macro (compiler-var term-var) body)
    (%macro (%lambda (compiler-var term-var) body)))

  (define-rule-syntax (let (id x) body)
    (%let ((id x)) body))

  ; TODO: Provide a type
  (define-rules-syntax
    ((lambda (id t) body)
      (abstraction 'id t (%lambda (id) body))))

  (define-rules-syntax
    ((prim id)
      (%native ($primitive 3 id)))
    ((prim id (p1 t1))
      (lambda (p1 t1)
        (term-apply (%native ($primitive 3 id)) p1)))
    ((prim id (p1 t1) (p2 t2))
      (lambda (p1 t1)
        (lambda (p2 t2)
          (switch p1
            ((native? $native-x)
              (switch p2
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
    ((curry p (p1 t1))
      (lambda (p1 t1)
        (switch p1
          ((native? $native) (native (p (native-ref $native))))
          ((else $other) (application (native p) $other)))))
    ((curry p (p1 t1) (p2 t2))
      (lambda (p1 t1)
        (lambda (p2 t2)
          (switch p1
            ((native? $native-x)
              (switch p2
                ((native? $native-y)
                  (native
                    (p
                      (native-ref $native-x)
                      (native-ref $native-y))))
                ((else $other-y)
                  (application (application (native p) $native-x) $other-y))))
            ((else $other-x)
              (application (application (native p) $other-x) p2)))))))

  (define-rules-syntax
    ((define-prim id p arg ...)
      (define id (curry p arg ...))))

  (define-rule-syntax (define-prims (id arg ... prim) ...)
    (begin (define-prim id arg ... prim) ...))

  (define (app $lhs $rhs)
    (switch $lhs
      ((type-abstraction? $type-abstraction)
        (type-abstraction-apply $type-abstraction $rhs))
      ((abstraction? $abstraction)
        (abstraction-apply $abstraction $rhs))
      ((else $other)
        (application (native $other) $rhs))))

  (define-prims
    (a-boolean 'a-boolean)
    (a-number  'a-number)
    (a-symbol  'a-symbol)
    (a-char    'a-char)
    (a-string  'a-string)

    (zero? %zero? (x a-number))

    (= %= (x a-number) (y a-number))
    (+ %+ (x a-number) (y a-number))
    (- %- (x a-number) (y a-number))
    (< %< (x a-number) (y a-number))

    (string-append %%string-append (a a-string) (b a-string))
    (string-length %%string-length (a a-string))
    (number->string %%number->string (a a-number)))

  (define-rules-syntax
    ((a-lambda (id in) out)
      (%pi 'id in (%lambda (id) out)))
    ((a-lambda in out)
      (%pi #f in (%lambda (id) out))))

  (define-rule-syntax (if cond true false)
    (switch cond
      ((native? $native)
        (%if (native-ref $native) true false))
      ((else $other-cond)
        (conditional $other-cond true false))))

)
