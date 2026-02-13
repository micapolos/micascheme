(library (micalang runtime)
  (export
    curry native app constant tagged
    any-type any-boolean any-number any-symbol any-char any-string
    = + - < zero?
    let lambda macro any-lambda app if)
  (import
    (except (micalang base) = + - < zero? lambda app let if string)
    (prefix (only (micalang base) let lambda app if zero? = + - <) %)
    (rename (micalang term) (pi %pi) (native %native) (constant %constant) (tagged %tagged) (macro %macro) (any-type %any-type)))
  (export
    (import
      (only (micascheme) equal? quote quasiquote unquote syntax unsyntax quasisyntax ... datum syntax-case)
      (prefix (micascheme) %%)))

  (define-rule-syntax (let (id x) ... body)
    (%let ((id x) ...) body))

  (define-rule-syntax (lambda (id _) body)
    (%lambda (id) body))

  (define-rule-syntax (native x) x)
  (define-rule-syntax (constant x) #f)
  (define-rule-syntax (tagged tag x) x)
  (define-rule-syntax (macro x ...) #f)

  (define-rules-syntax
    ((any-lambda . _) #f))

  (define-rules-syntax
    ((curry x)
      x)
    ((curry x (p1 t1))
      (lambda (p1 t1) (x p1)))
    ((curry x (p1 t1) (p2 t2))
      (lambda (p1 t1) (lambda (p2 t2) (x p1 p2))))
    ((curry x (p1 t1) (p2 t2) (p3 t3))
      (lambda (p1 t1) (lambda (p2 t2) (lambda (p3 t3) (x p1 p2 p3)))))
    ((curry x (p1 t1) (p2 t2) (p3 t3) (p4 t4))
      (lambda (p1 t1) (lambda (p2 t2) (lambda (p3 t3) (lambda (p4 t4) (x p1 p2 p3 p4)))))))

  (define-rules-syntax
    ((prim id)
      ($primitive 3 id))
    ((prim id (p1 t1))
      (lambda (p1 t1) (($primitive 3 id) p1)))
    ((prim id (p1 t1) (p2 t2))
      (lambda (p1 t1) (lambda (p2 t2) (($primitive 3 id) p1 p2))))
    ((prim id (p1 t1) (p2 t2) (p3 t3))
      (lambda (p1 t1) (lambda (p2 t2) (lambda (p3 t3) (($primitive 3 id) p1 p2 p3)))))
    ((prim id (p1 t1) (p2 t2) (p3 t3) (p4 t4))
      (lambda (p1 t1) (lambda (p2 t2) (lambda (p3 t3) (lambda (p4 t4) (($primitive 3 id) p1 p2 p3 p4)))))))

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
    (any-type    #f)
    (any-boolean #f)
    (any-number  #f)
    (any-symbol  #f)
    (any-char    #f)
    (any-string  #f)

    (zero? %zero? (x any-number))

    (= %= (x any-number) (y any-number))
    (+ %+ (x any-number) (y any-number))
    (- %- (x any-number) (y any-number))
    (< %< (x any-number) (y any-number)))
)
