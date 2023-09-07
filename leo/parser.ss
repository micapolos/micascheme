(library (leo parser)
  (export 
    script-parser
    line-parser)
  (import (micascheme) (parser))

  (data (env value atom-parser))

  (define (default-env)
    (env (stack) (atom-parser)))

  (define script-parser
    (case-lambda
      (() (script-parser (default-env)))
      (($env)
        (parser-map
          (skip-empty-lines-parser
            (stack-parser
              (newline-ended-parser (line-parser $env `multiline))))
          reverse))))

  (define (line-stack-parser $env)
    (separated-stack-parser
      (line-parser $env `multiline)
      (newline-parser)))

  (define line-parser
    (case-lambda
      (() (line-parser (default-env) `multiline))
      (($env $mode)
        (oneof-parser
          (env-atom-parser $env)
          (field-parser $env $mode)))))

  (define (atom-parser)
    (oneof-parser
      (literal-string-parser)
      (integer-parser)))

  (define (field-parser $env $mode)
    (parser-lets
      ($word (word-parser))
      ($rhs (rhs-parser $env $mode))
      (cond
        ((null? $rhs) (parser $word))
        (else (parser (cons $word $rhs))))))

  (define (rhs-parser $env $mode)
    (case $mode
      ((multiline)
        (oneof-parser
          (parser (list))
          (space-rhs-parser $env $mode)
          (parenthesized-rhs-parser $env)
          (newline-rhs-parser $env)
          (colon-rhs-parser $env $mode)))
      ((parenthesis colon)
        (oneof-parser
          (parser (list))
          (space-rhs-parser $env $mode)
          (parenthesized-rhs-parser $env)
          (colon-rhs-parser $env $mode)))))

  (define (space-rhs-parser $env $mode)
    (parser-lets
      (skip (space-parser))
      ($line (line-parser $env $mode))
      (parser (list $line))))

  (define (newline-rhs-parser $env)
    (parser-lets
      (skip (newline-parser))
      ($line-stack (indent-parser (line-stack-parser $env)))
      (parser (reverse $line-stack))))

  (define (colon-rhs-parser $env $mode)
    (parser-lets
      (skip (colon-separator-parser))
      ($line-stack
        (case $mode
          ((multiline colon)
            (non-empty-separated-stack-parser
              (line-parser $env `colon)
              (comma-separator-parser)))
          ((parenthesis)
            (parser-lets
              ($line (line-parser $env `colon))
              (parser (stack $line))))))
      (parser (reverse $line-stack))))

  (define (parenthesized-rhs-parser $env)
    (parser-lets
      ($line-stack
        (parenthesized-parser
          (separated-stack-parser
            (line-parser $env `parenthesis)
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
