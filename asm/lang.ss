(library (asm lang)
  (export
    define-op-syntax
    define-op
    define-ops
    define-asm
    define-fragment
    define-fragments
    define-value
    define-expression-syntax
    define-expression
    define-constant
    define-constants
    define-struct
    proc data
    block
    (rename
      (%define define)
      (%define-values define-values))
    db dw db-e dz utf8 ascii
    with-labels
    reverse
    asm
    check-asm)
  (import
    (rename (asm base)
      (data %data)
      (reverse %reverse))
    (asm syntax-block)
    (asm block-fragment)
    (asm expression)
    (except (asm block) block)
    (asm syntax-expression)
    (asm fragment)
    (asm linked)
    (asm assembled)
    (asm environment)
    (asm environmental)
    (asm dependencies)
    (asm relocable)
    (except (asm assembled) assembled)
    (asm org))
  (export
    (import
      (only (asm base)
        lambda datum
        define-keywords
        define-syntax syntax quasisyntax unsyntax unsyntax-splicing
        syntax-case syntax-rules
        keywords
        begin
        + - *
        bitwise-and bitwise-ior bitwise-xor
        fx+ fx- fxnot fxand fxior fxxor fxsrl fxsll
        logging
        length
        export
        ...
        comment)
      (only (asm syntax-block) align trace-block-expansion)
      (asm linked)
      (asm org)))

  (define-rules-syntax
    ((define-op-syntax (id $syntax) body)
      (define-op-syntax id
        (lambda ($syntax) body)))
    ((define-op-syntax id proc)
      (define-syntax id (make-compile-time-value proc))))

  (define-rules-syntax (literals keywords)
    ((define-op (keywords keyword ...) pattern body ...)
      (define-ops (keywords keyword ...) (pattern body ...)))
    ((define-op pattern body ...)
      (define-op (keywords) pattern body ...)))

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
                              #`(pattern #'(begin body ...)))))
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

  (define-rule-syntax (define-fragments (id body ...) ...)
    (begin (define-fragment id body ...) ...))

  (define-rule-syntax (define-value id expr)
    (%define id expr))

  (define-rule-syntax (define-expression-syntax id transformer)
    (define-syntax id
      (make-compile-time-value transformer)))

  (define-rule-syntax (define-expression (id param ...) body)
    (define-expression-syntax id
      (syntax-rules ()
        ((_ param ...) body))))

  (define-rule-syntax (define-constant id expr)
    (define-syntax id (eval 'expr (environment '(scheme)))))

  (define-rule-syntax (define-constants (id expr) ...)
    (begin (define-constant id expr) ...))

  (define-rule-syntax (%define-values (id expr) ...)
    (begin (define-value id expr) ...))

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

  (define-syntax dz
    (make-compile-time-value
      (lambda ($syntax)
        (lambda ($lookup)
          (syntax-case $syntax ()
            ((_ s)
              (string? (datum s))
              (dz-block (datum s))))))))

  (define-syntax utf8
    (make-compile-time-value
      (lambda ($syntax)
        (lambda ($lookup)
          (syntax-case $syntax ()
            ((_ x ...)
              (for-all string? (map syntax->datum #'(x ...)))
              (apply utf8-block (map syntax->datum #'(x ...)))))))))

  (define-syntax ascii
    (make-compile-time-value
      (lambda ($syntax)
        (lambda ($lookup)
          (syntax-case $syntax ()
            ((_ x ...)
              (for-all string? (map syntax->datum #'(x ...)))
              (apply ascii-block (map syntax->datum #'(x ...)))))))))

  (define-syntax with-labels
    (make-compile-time-value
      (lambda ($syntax)
        (lambda ($lookup)
          (syntax-case $syntax ()
            ((_ (id ...) x ...)
              (for-all identifier? #'(id ...))
              (syntax-replace-all
                #'(id ...)
                (generate-temporaries #'(id ...))
                #'(begin x ...))))))))

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

  (define-rule-syntax (define-struct x ...)
    (begin))
)
