(library (leo parser)
  (export 
    script-parser
    line-parser)
  (import (micascheme) (parser))

  (define (script-parser)
    (parser-map (line-stack-parser) reverse))

  (define (line-stack-parser)
    (separated-stack-parser
      (line-parser)
      (newline-parser)))

  (define (line-parser)
    (oneof-parser
      (atom-parser)
      (field-parser)))

  (define (atom-parser)
    (oneof-parser
      (literal-string-parser)
      (integer-parser)))

  (define (field-parser)
    (parser-lets
      ($word (word-parser))
      ($rhs (rhs-parser))
      (cond
        ((null? $rhs) (parser $word))
        (else (parser (cons $word $rhs))))))

  (define (rhs-parser)
    (oneof-parser
      (parser (list))
      (space-rhs-parser)
      (newline-rhs-parser)
      (colon-rhs-parser)))

  (define (space-rhs-parser)
    (parser-lets 
      (skip (space-parser))
      ($line (line-parser))
      (parser (list $line))))

  (define (newline-rhs-parser)
    (parser-lets
      (skip (newline-parser))
      (indent-parser (script-parser))))

  (define (colon-rhs-parser)
    (parser-lets
      (skip (colon-parser))
      (skip (space-parser))
      ($line-stack
        (non-empty-separated-stack-parser
          (simple-line-parser)
          (parser-lets
            (skip (comma-parser))
            (skip (space-parser))
            (parser #t))))
      (parser (reverse $line-stack))))

  (define (simple-line-parser)
    (oneof-parser
      (atom-parser)
      (simple-field-parser)))

  (define (simple-field-parser)
    (parser-lets
      ($word (word-parser))
      ($rhs (simple-rhs-parser))
      (cond
        ((null? $rhs) (parser $word))
        (else (parser (cons $word $rhs))))))

  (define (simple-rhs-parser)
    (oneof-parser
      (parser (list))
      (simple-space-rhs-parser)))

  (define (simple-space-rhs-parser)
    (parser-lets
      (skip (space-parser))
      ($line (simple-line-parser))
      (parser (list $line))))
)
