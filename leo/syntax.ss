(library
  (leo syntax)
  (export
    match
    with-syntax
    with-syntax*
    with-identifier
    with-identifier*
    syntax-rules
    pattern-id
    clause-id
    rule->clause
    group-clauses)
  (import
    (rename (scheme)
      (with-syntax %with-syntax)
      (with-implicit %with-implicit)
      (syntax-rules %syntax-rules))
    (leo in)
    (leo with)
    (keyword)
    (syntax-keywords)
    (syntax)
    (list)
    (syntaxes))
  (export
    (import
      (only (scheme)
        syntax
        quasisyntax
        unsyntax
        unsyntax-splicing
        with-implicit)
      (rename
        (only (syntax)
          syntax?
          syntax=?
          define-keyword
          define-keywords)
        (syntax=? free-syntax=?))))

  (define-keyword match)

  (define-rules-syntaxes
    (keywords keywords match in when)

    ((with-syntax (match pattern expr) ... (in x xs ...))
      (%with-syntax ((pattern expr) ...) x xs ...))

    ((with-identifier (id expr) ... (in x xs ...))
      (for-all identifier? #'(id ...))
      (%with-syntax ((id expr) ...) x xs ...))

    ((with-syntax* (in x xs ...)) (begin x xs ...))
    ((with-syntax* (match pattern expr) rest ... (in x xs ...))
      (with-syntax (match pattern expr)
        (in (with-syntax* rest ... (in x xs ...)))))

    ((with-identifier* (in x xs ...)) (begin x xs ...))
    ((with-identifier* (id expr) rest ... (in x xs ...))
      (keyword? id)
      (with-identifier (id expr) (in (with-identifier* rest ... (in x xs ...)))))

    ((syntax-rules (keywords ks ...) (when pattern x xs ...) ...)
      (%syntax-rules (ks ...) (pattern x xs ...) ...))

    ((syntax-rules (when pattern x xs ...) ...)
      (syntax-rules (keywords) (when pattern x xs ...) ...)))

  (define (pattern-id pattern)
    (syntax-case pattern ()
      (id (keyword? id) #'id)
      ((id . x) (keyword? id) #'id)))

  (define (clause-id clause)
    (syntax-case clause (when)
      ((when pattern . x) (pattern-id #'pattern))))

  (define (rule->clause rule)
    (syntax-case rule (when)
      ((when pattern body)
        #'(when pattern #'body))
      ((when pattern fender body)
        #'(when pattern fender #'body))))

  (define (group-clauses stx)
    (syntax-case stx ()
      ((clause ...)
        (group-by clause-id free-identifier=? #'(clause ...)))))
)
