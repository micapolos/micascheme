(library (leo parser)
  (export 
    script-parser
    line-parser)
  (import (micascheme) (parser))

  (define (script-parser)
    (parser-map
      (skip-empty-lines-parser
        (stack-parser
          (newline-ended-parser (line-parser))))
      reverse))

  (define (line-stack-parser)
    (separated-stack-parser
      (line-parser `multiline)
      (newline-parser)))

  (define line-parser
    (case-lambda
      (() (line-parser `multiline))
      (($mode)
        (oneof-parser
          (atom-parser)
          (field-parser $mode)))))

  (define (atom-parser)
    (oneof-parser
      (literal-string-parser)
      (integer-parser)))

  (define (field-parser $mode)
    (parser-lets
      ($word (word-parser))
      ($rhs (rhs-parser $mode $word))
      (cond
        ((null? $rhs) (parser $word))
        (else (parser (cons $word $rhs))))))

  (define (rhs-parser $mode $word)
    (case $mode
      ((multiline)
        (oneof-parser
          (parser (list))
          (space-rhs-parser $mode $word)
          (parenthesized-rhs-parser)
          (newline-rhs-parser)
          (colon-rhs-parser $mode)))
      ((parenthesis colon)
        (oneof-parser
          (parser (list))
          (space-rhs-parser $mode $word)
          (parenthesized-rhs-parser)
          (colon-rhs-parser $mode)))))

  (define (space-rhs-parser $mode $word)
    (parser-lets 
      (skip (space-parser))
      ($line
        (case $word
          ((text) (parser-until-newline (string-parser)))
          (else (line-parser $mode))))
      (parser (list $line))))

  (define (newline-rhs-parser)
    (parser-lets
      (skip (newline-parser))
      ($line-stack (indent-parser (line-stack-parser)))
      (parser (reverse $line-stack))))

  (define (colon-rhs-parser $mode)
    (parser-lets
      (skip (colon-separator-parser))
      ($line-stack
        (case $mode
          ((multiline colon)
            (non-empty-separated-stack-parser
              (line-parser `colon)
              (comma-separator-parser)))
          ((parenthesis)
            (parser-lets
              ($line (line-parser `colon))
              (parser (stack $line))))))
      (parser (reverse $line-stack))))

  (define (parenthesized-rhs-parser)
    (parser-lets
      ($line-stack
        (parenthesized-parser
          (separated-stack-parser
            (line-parser `parenthesis)
            (comma-separator-parser))))
      (parser (reverse $line-stack))))

  (define (comma-separator-parser)
    (parser-lets
      (skip (comma-parser))
      (skip (space-parser))
      (parser #t)))

  (define (colon-separator-parser)
    (parser-lets
      (skip (colon-parser))
      (skip (space-parser))
      (parser #t)))

  (define (parenthesized-parser $parser)
    (parser-lets
      (skip (exact-char-parser #\())
      ($item $parser)
      (skip (exact-char-parser #\)))
      (parser $item)))
)
