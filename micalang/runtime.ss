(library (micalang runtime)
  (export
    prim
    literal app
    type boolean number symbol char string
    = + - < zero?
    list let lambda pi app if)
  (import
    (except (micalang base) = + - < zero? list lambda app let if string)
    (prefix (only (micalang base) let lambda app if zero? = + - <) %)
    (rename (micalang term) (pi %pi)))
  (export
    (import
      (only (micascheme) equal? quote)
      (only (micalang term) native)))

  (define-rule-syntax (let (id x) ... body)
    (%let ((id x) ...) body))

  (define-rule-syntax (lambda id body)
    (%lambda (id) body))

  (define-rules-syntax
    ((pi (id in) out)
      (%lambda (id) out))
    ((pi in out)
      (pi (_ in) out)))

  (define-rules-syntax
    ((curry x) x)
    ((curry a x) (lambda a (x a)))
    ((curry a b x) (lambda a (lambda b (x a b))))
    ((curry a b c x) (lambda a (lambda b (lambda c (x a b c)))))
    ((curry a b c d x) (lambda a (lambda b (lambda c (lambda d (x a b c d)))))))

  (define-rules-syntax
    ((prim id) ($primitive 3 id))
    ((prim id a) (lambda a (($primitive 3 id) a)))
    ((prim id a b) (lambda a (lambda b (($primitive 3 id) a b))))
    ((prim id a b c) (lambda a (lambda b (lambda c (($primitive 3 id) a b c)))))
    ((prim id a b c d) (lambda a (lambda b (lambda c (lambda d (($primitive 3 id) a b c d)))))))

  (define-rules-syntax
    ((define-curry id arg ... p)
      (define id (curry arg ... p))))

  (define-rule-syntax (define-currys (id arg ... prim) ...)
    (begin (define-curry id arg ... prim) ...))

  (define-rules-syntax
    ((app lhs rhs)
      (%app lhs rhs)))

  (define-rule-syntax (literal x) x)

  (define-rule-syntax (if cond true false)
    (%if cond true false))

  (define-currys
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

  (define list (lambda x (application list x)))
)
