(library (asm asm)
  (export
    define-asm
    define-asm-rules
    syntax->asm
    syntaxes->asm
    asm-binary
    asm-bytevector
    check-asm
    org)
  (import
    (micascheme)
    (only (asm block) block-binary-syntax empty-block)
    (binary)
    (only (asm typed) syntax->expr))

  (define-keywords org)

  (define-rules-syntax
    ((define-asm id asm)
      (identifier? #'id)
      (define-syntax id (make-compile-time-value asm)))
    ((define-asm (id $lookup $syntax) body)
      (and (identifier? #'$lookup) (identifier? #'$syntax))
      (define-asm id (lambda ($lookup $syntax) body))))

  (define-syntax (define-asm-rules $syntax)
    (syntax-case $syntax (keywords)
      ((_ (keywords keyword ...) clause ...)
        #`(begin
          #,@(map-with
            ($group (group-by syntax-clause-id free-identifier=? #'(clause ...)))
            (lets
              ((pair $id $clauses) $group)
              #`(define-asm (#,$id $lookup $syntax)
                (syntax-case $syntax (keyword ...)
                  #,@(map-with ($clause $clauses)
                    (syntax-case $clause ()
                      ((pattern body ...)
                        #`(pattern (syntaxes->asm $lookup (list #'body ...))))))))))))
      ((_ clause ...)
        #`(define-asm-rules (keywords) clause ...))))

  (meta define (syntax->asm $lookup $syntax)
    (lets
      ($identifier
        (or
          (syntax-selector $syntax)
          (syntax-error $syntax "invalid asm")))
      (app
        (or
          ($lookup $identifier)
          (syntax-error $identifier "undefined asm"))
        $lookup $syntax)))

  (meta define (syntaxes->asm $lookup $syntaxes)
    (lambda ($block)
      (fold-left
        (lambda ($block $asm) ($asm $block))
        $block
        (map (partial syntax->asm $lookup) $syntaxes))))

  (define-syntax (asm-binary $syntax $lookup)
    (syntax-case $syntax (org)
      ((_ (org $org) asm ...)
        (syntax->expr $lookup #'binary
          (block-binary-syntax
            (fold-left
              (lambda ($block $asm) ((syntax->asm $lookup $asm) $block))
              (empty-block (datum $org))
              #'(asm ...)))))
      ((_ asm ...)
        #`(asm-binary (org 0) asm ...))))

  (define-rule-syntax (asm-bytevector asm ...)
    (binary->bytevector (asm-binary asm ...)))

  (define-rule-syntax (check-asm in out)
    (check (equal? (asm-bytevector in) (asm-bytevector out))))
)
