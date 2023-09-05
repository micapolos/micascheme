(library (leo parser)
  (export 
    script-parser
    line-parser)
  (import (micascheme) (parser))

  (define (script-parser)
    (parser-map (line-stack-parser) reverse))

  (define (line-stack-parser)
    (separator-stack-parser
      (line-parser)
      (newline-parser)))

  (define (line-parser)
    (oneof-parser
      (atom-parser)
      (field-parser)))

  (define (atom-parser)
    (oneof-parser
      (literal-string-parser)
      (positive-integer-parser)))

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
      (newline-rhs-parser)))

  (define (space-rhs-parser)
    (parser-lets 
      (skip (space-parser))
      ($line (line-parser))
      (parser (list $line))))

  (define (newline-rhs-parser)
    (parser-lets
      (skip (newline-parser))
      (indent-parser (script-parser))))
)
