(library (leo parser)
  (export 
    script-parser
    line-parser)
  (import 
    (micascheme) 
    (except (parser) line-parser))

  (define (script-parser)
    (parser-map
      (separator-stack-parser 
        (line-parser)
        (exact-char-parser #\newline))
      reverse))

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
    (parser-map
      (parser-lets (skip (exact-char-parser #\space)) (line-parser))
      list))

  (define (newline-rhs-parser)
    (parser-lets
      (skip (exact-char-parser #\newline))
      (indent-parser (script-parser))))
)