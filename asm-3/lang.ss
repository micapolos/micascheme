(library (asm-3 lang)
  (export
    proc data const
    + -
    assembled
    check-assembled)
  (import
    (rename
      (except (asm-3 base) data begin)
      (+ %+)
      (- %-))
    (asm-3 block-syntax)
    (asm-3 expression-syntax)
    (asm-3 block-fragment)
    (asm-3 linker)
    (except (asm-3 linked) assembled)
    (asm-3 org))
  (export
    (import
      (asm-3 org)
      (asm-3 expression-syntax)
      (asm-3 block-syntax)))

  (define-rule-syntax (define-asm id value)
    (define-syntax id (make-compile-time-value value)))

  (define-rule-syntax (proc id x ...)
    (define-asm id (block->fragment (begin x ...))))

  (define-rule-syntax (data id x ...)
    (define-asm id (block->fragment (begin x ...))))

  (define-rule-syntax (const id x)
    (define-asm id #'(expr x)))

  (define-expr + %+)
  (define-expr - %-)

  (define-syntax (assembled-proc $syntax $lookup)
    (syntax-case $syntax (org)
      ((_ (org $org) id)
        (and
          (integer? (datum $org))
          (identifier? #'id))
        (assembled->syntax (assemble-identifier $lookup (datum $org) #'id)))))

  (define-syntax (assembled $syntax $lookup)
    (syntax-case $syntax (org)
      ((_ (org $org) x ...)
        (integer? (datum $org))
        #`(let ()
          (proc main x ...)
          (assembled-proc (org $org) main)))))

  (define-rule-syntax (check-assembled (org $org) x ... out)
    (check (equal? (assembled->datum (assembled (org $org) x ...)) 'out)))
)
