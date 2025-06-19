(library (asm-2 typed)
  (export
    void type boolean integer char string procedure
    typed typed-type typed-value
    syntax->typed
    define-typed)
  (import (micascheme) (syntax lookup))

  (define-keywords typed type boolean integer char procedure)

  (define-rules-syntax (literals typed)
    ((define-typed id (typed type expr))
      (define-typed id
        (lambda ($lookup $syntax)
          (syntax-case $syntax (id)
            (id #'(typed type expr))))))
    ((define-typed (id $lookup $syntax) body)
      (and (identifier? #'$lookup) (identifier? #'$syntax))
      (define-syntax id
        (lambda ($lookup $syntax) body)))
    ((define-typed id type/proc)
      (identifier? #'id)
      (define-syntax id
        (make-compile-time-value type/proc))))

  (define (syntax->typed $lookup $syntax)
    (syntax-case $syntax (void type boolean integer char string procedure lambda)
      (void #`(typed type void))
      (type #`(typed type type))
      (boolean #`(typed type boolean))
      (integer #`(typed type integer))
      (char #`(typed type char))
      (string #`(typed type string))
      ((procedure params result)
        #`(typed type
          (procedure
            #,(map*
              (partial syntax->expr $lookup #'type)
              (partial syntax->expr $lookup #'type)
              (syntax->list* #'params))
            #,(syntax->expr $lookup #'type #'result))))
      ((void)
        #`(typed void (void)))
      (b
        (boolean? (datum b))
        #`(typed boolean b))
      (i
        (integer? (datum i))
        #`(typed integer i))
      (ch
        (char? (datum ch))
        #`(typed char ch))
      (str
        (string? (datum str))
        #`(typed string str))
      ((lambda ((typ id) ...) body)
        (for-all identifier? #'(id ...))
        (lets
          ($types (map (partial syntax->expr $lookup #'type) #'(typ ...)))
          ($ids #'(id ...))
          ($typed-body
            (syntax->typed
              (fold-left lookup+undefined $lookup $ids $types)
              #'body))
          #`(typed
            (procedure (#,@$types) #,(typed-type $typed-body))
            (lambda (#,@$ids) #,(typed-value $typed-body)))))
      ((fn arg ...)
        (syntax-case (syntax->typed $lookup #'fn) (typed procedure)
          ((typed (procedure params result) fn-expr)
            (syntax-case
              (map*
                (partial syntax->expr $lookup)
                (partial syntaxes->exprs $lookup)
                (syntax->list* #'params)
                #'(arg ...))
              ()
              ((arg-expr ...)
                #`(typed result (fn-expr arg-expr ...)))))
          ((typed _ _)
            (syntax-error #'fn "not an procedure"))))
      (id
        (identifier? #'id)
        (switch (lookup-ref $lookup #'id)
          ((procedure? $procedure)
            ($procedure $lookup #'id))
          ((else $type)
            #`(typed #,$type id))))))

  (define (syntax->expr $lookup $type $syntax)
    (syntax-case (syntax->typed $lookup $syntax) (typed)
      ((typed type expr)
        (cond
          ((type=? #'type $type) #'expr)
          (else
            (syntax-error $syntax
              (format "invalid type ~s, expected ~s, in"
                (syntax->datum #'type)
                (syntax->datum $type))))))))

  (define (syntaxes->exprs $lookup $type $syntaxes)
    (map (partial syntax->expr $lookup $type) $syntaxes))

  (define (type=? $type-a $type-b)
    (equal?
      (syntax->datum $type-a)
      (syntax->datum $type-b)))

  (define (typed-type $typed)
    (syntax-case $typed (typed)
      ((typed type value) #'type)))

  (define (typed-value $typed)
    (syntax-case $typed (typed)
      ((typed type value) #'value)))
)
