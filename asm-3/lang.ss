(library (asm-3 lang)
  (export
    define-op-syntax
    define-op
    define-ops
    define-asm
    proc data
    block
    (rename (%define define))
    db dw db-e
    with-labels
    reverse
    asm
    check-asm)
  (import
    (rename (asm-3 base)
      (data %data)
      (reverse %reverse))
    (asm-3 syntax-block)
    (asm-3 block-fragment)
    (asm-3 expression)
    (except (asm-3 block) block)
    (asm-3 syntax-expression)
    (asm-3 fragment)
    (asm-3 linked)
    (asm-3 assembled)
    (asm-3 environment)
    (asm-3 environmental)
    (asm-3 dependencies)
    (asm-3 relocable)
    (except (asm-3 assembled) assembled)
    (asm-3 org))
  (export
    (import
      (only (asm-3 base)
        define-keywords
        keywords
        syntax
        begin
        + -
        bitwise-and bitwise-ior bitwise-xor
        fx+ fx- fxnot fxand fxior fxxor fxsrl fxsll
        ...)
      (only (asm-3 syntax-block) align trace-block-expansion)
      (asm-3 org)))

  (define-rules-syntax
    ((define-op-syntax (id $syntax) body)
      (define-op-syntax id
        (lambda ($syntax) body)))
    ((define-op-syntax id proc)
      (define-syntax id (make-compile-time-value proc))))

  (define-rules-syntax (literals keywords)
    ((define-op (keywords keyword ...) pattern body )
      (define-ops (keywords keyword ...) (pattern body)))
    ((define-op pattern body)
      (define-op (keywords) pattern body)))

  (define-syntax (define-ops $syntax)
    (syntax-case $syntax (keywords)
      ((_ (keywords keyword ...) clause ...)
        #`(begin
          #,@(map
            (lambda ($clauses-group)
              (lets
                ((pair $id $clauses) $clauses-group)
                #`(define-op-syntax #,$id
                  (lambda ($syntax)
                    (syntax-case $syntax (keyword ...)
                      #,@(map
                        (lambda ($clause)
                          (syntax-case $clause ()
                            ((pattern body ...)
                              #'(pattern #'(begin body ...)))))
                        $clauses))))))
            (group-by
              syntax-clause-id
              free-identifier=?
              #'(clause ...)))))
      ((_ clause ...)
        #'(define-ops (keywords) clause ...))))

  (define-rule-syntax (define-dependent id dependent)
    (define-syntax id (make-compile-time-value dependent)))

  (define-syntax (define-fragment $syntax $lookup)
    (syntax-case $syntax ()
      ((_ id x ...)
        #`(define-dependent id
          #,(fragment->syntax
            (block->fragment
              (syntax->block $lookup #'(begin x ...))))))))

  (define-rule-syntax (define-asm id x ...)
    (define-fragment id x ...))

  (define-rule-syntax (block id x ...)
    (define-asm id x ...))

  (define-rule-syntax (proc id x ...)
    (define-asm id x ...))

  (define-rule-syntax (data id x ...)
    (define-asm id x ...))

  (define-syntax (%define $syntax $lookup)
    (syntax-case $syntax ()
      ((_ id x)
        #`(define-dependent id
          #,(expression->syntax
            (syntax->expression $lookup #'x))))))

  (define-syntax db
    (make-compile-time-value
      (lambda ($syntax)
        (lambda ($lookup)
          (syntax-case $syntax ()
            ((_ x ...)
              (apply db-block
                (map (partial syntax->expression $lookup) #'(x ...)))))))))

  (define-syntax dw
    (make-compile-time-value
      (lambda ($syntax)
        (lambda ($lookup)
          (syntax-case $syntax ()
            ((_ x ...)
              (apply dw-block
                (map (partial syntax->expression $lookup) #'(x ...)))))))))

  (define-syntax with-labels
    (make-compile-time-value
      (lambda ($syntax)
        (lambda ($lookup)
          (syntax-case $syntax ()
            ((_ (id ...) x ...)
              (for-all identifier? #'(id ...))
              (fold-left
                (lambda ($syntax $from $to)
                  (syntax-replace $from $to $syntax))
                #'(begin x ...)
                #'(id ...)
                (generate-temporaries #'(id ...)))))))))

  (define-syntax reverse
    (make-compile-time-value
      (lambda ($syntax)
        (lambda ($lookup)
          (syntax-case $syntax ()
            ((_ x ...)
              #`(begin #,@(%reverse #'(x ...)))))))))

  (define-ops
    ((db-e x)
      (with-labels (tmp)
        (db (bitwise-and #xff (- x tmp)))
        tmp)))

  (define-syntax (asm $syntax $lookup)
    (syntax-case $syntax (org)
      ((_ (org $org) x ...)
        (integer? (datum $org))
        (lets
          ($main-fragment
            (block->fragment
              (syntax->block $lookup #'(begin x ...))))
          ($lookup (lookup+ $lookup #'main $main-fragment))
          ($dependencies (resolve-dependencies $lookup #'main))
          ($linked (list->linked $dependencies))
          ($main-offset (environment-ref (environmental-environment $linked) #'main))
          ($assembled
            #`(assembled
              (+ $org #,$main-offset)
              (relocable-ref #,(environmental-ref $linked) $org)))
          ;(run (pretty-print (syntax->datum $assembled)))
          $assembled))))

  (define (binary->db-datum $binary)
    `(db ,@(bytevector->u8-list (binary->bytevector $binary))))

  (define (asm->datum $asm)
    `(asm
      (start ,(assembled-start $asm))
      ,(binary->db-datum (assembled-ref $asm))))

  (define-rule-syntax (org) (check-asm x ... out)
    (check (equal? (asm->datum (asm x ...)) 'out)))
)
