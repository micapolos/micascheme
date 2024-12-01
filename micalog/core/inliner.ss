(library (micalog core inliner)
  (export
    inline-expr
    lookup+core)
  (import
    (micascheme)
    (syntax lookup)
    (prefix (micalog core keywords) %))

  (data (expr inliner))

  (define (inline-expr $lookup $expr)
    (syntax-case $expr ()
      ((type term)
        #`(type
          #,(syntax-case #'term (%variable)
            (integer (nonnegative-integer? (datum integer))
              #'integer)
            ((id x ...)
              (fluent $lookup
                (lookup-expr-inliner (identifier id))
                (app $lookup #'term)))
            (id
              (fluent $lookup
                (lookup-expr-inliner (identifier id))
                (app $lookup #'term))))))))

  (define (lookup-expr-inliner $lookup $id)
    (switch (lookup-ref $lookup $id)
      ((expr? $expr)
        (expr-inliner $expr))
      ((else $other)
        (syntax-error $id "not expression"))))

  (define (lookup+core $lookup)
    (lookup+defs $lookup inline
      (expr (%variable name)
        #`(%variable name))
      (expr (%append x ...)
        #`(%append
          #,@(inline (exprs x ...))))
      (expr (%take x size)
        #`(%take #,(inline (expr x)) size))))

  (define-aux-keyword exprs)

  (define-case-syntax (lookup+defs lookup inline def ...)
    #`(fluent lookup
      #,@(map
        (lambda ($def)
          (syntax-case $def ()
            ((kind (name param ...) body)
              #`(lookup+undefined #'name
                (kind
                  (lambda (lookup $expr)
                    (define-syntax inline
                      (syntax-rules (expr *)
                        ((_ (expr x))
                          (inline-expr lookup #'x))
                        ((_ (exprs x (... ...)))
                          (map
                            (partial inline-expr lookup)
                            (syntaxes x (... ...))))))
                    (syntax-case $expr ()
                      ((name param ...) body))))))))
        (syntaxes def ...))))
)
