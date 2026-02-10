(library (micalang runtime)
  (export
    curry native app constant tagged
    boolean number symbol char string
    = + - < zero?
    list let lambda macro pi app if)
  (import
    (except (micalang base) = + - < zero? list lambda app let if string)
    (prefix (only (micalang base) let lambda app if zero? = + - <) %)
    (rename (micalang term) (pi %pi) (native %native) (constant %constant) (tagged %tagged) (macro %macro)))
  (export
    (import
      (only (micascheme) equal? quote)
      (prefix (micascheme) %%)))

  (define-rule-syntax (let (id x) ... body)
    (%let ((id x) ...) body))

  (define-rule-syntax (lambda id body)
    (%lambda (id) body))

  (define-rule-syntax (native x) x)
  (define-rule-syntax (constant x) #f)
  (define-rule-syntax (tagged tag x) x)
  (define-rule-syntax (macro x ...) #f)

  (define-rules-syntax
    ((pi (id in) out)
      (%lambda (id) out))
    ((pi in out)
      (pi (_ in) out)))

  (define-rules-syntax
    ((curry x) x)
    ((curry x a) (lambda a (x a)))
    ((curry x a b) (lambda a (lambda b (x a b))))
    ((curry x a b c) (lambda a (lambda b (lambda c (x a b c)))))
    ((curry x a b c d) (lambda a (lambda b (lambda c (lambda d (x a b c d)))))))

  (define-rules-syntax
    ((prim id) ($primitive 3 id))
    ((prim id a) (lambda a (($primitive 3 id) a)))
    ((prim id a b) (lambda a (lambda b (($primitive 3 id) a b))))
    ((prim id a b c) (lambda a (lambda b (lambda c (($primitive 3 id) a b c)))))
    ((prim id a b c d) (lambda a (lambda b (lambda c (lambda d (($primitive 3 id) a b c d)))))))

  (define-rules-syntax
    ((define-curry id p arg ...)
      (define id (curry p arg ...))))

  (define-rule-syntax (define-currys (id arg ... prim) ...)
    (begin (define-curry id arg ... prim) ...))

  (define-rules-syntax
    ((app lhs rhs)
      (%app lhs rhs)))

  (define-rule-syntax (if cond true false)
    (%if cond true false))

  (define-currys
    (boolean 'boolean)
    (number  'number)
    (symbol  'symbol)
    (char    'char)
    (string  'string)

    (zero? %zero? x)

    (= %= x y)
    (+ %+ x y)
    (- %- x y)
    (< %< x y))

  (define list (lambda x (application list x)))
)
