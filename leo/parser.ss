(library (leo parser)
  (export 
    script-parser
    line-parser
    parse-script
    read-script
    load-script)
  (import
    (micascheme)
    (parser)
    (scheme parser))

  (data (env value atom-parser))

  (define (parse-script $string)
    (parse (script-parser) $string))

  (define (read-script $port)
    (parse-script (get-string-all $port)))

  (define (load-script $filename)
    (call-with-input-file $filename read-script))

  (define (default-env)
    (env (stack) (atom-parser)))

  (define script-parser
    (case-lambda
      (() (script-parser (default-env)))
      (($env)
        (lets
          ((parser $lines)
            (skip-empty-lines-parser
              (stack-parser
                (newline-ended-parser (line-parser $env `multiline)))))
          (parser (reverse $lines))))))

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
    (lets
      ((parser $word) (word-parser))
      ((parser $rhs) (rhs-parser $env $mode))
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
    (lets
      ((parser _) (space-parser))
      ((parser $line) (line-parser $env $mode))
      (parser (list $line))))

  (define (newline-rhs-parser $env)
    (lets
      ((parser _) (newline-parser))
      ((parser $line-stack) (indent-parser (line-stack-parser $env)))
      (parser (reverse $line-stack))))

  (define (colon-rhs-parser $env $mode)
    (lets
      ((parser _) (colon-separator-parser))
      ((parser $line-stack)
        (case $mode
          ((multiline colon)
            (non-empty-separated-stack-parser
              (line-parser $env `colon)
              (comma-separator-parser)))
          ((parenthesis)
            (lets
              ((parser $line) (line-parser $env `colon))
              (parser (stack $line))))))
      (parser (reverse $line-stack))))

  (define (parenthesized-rhs-parser $env)
    (lets
      ((parser $line-stack)
        (parenthesized-parser
          (separated-stack-parser
            (line-parser $env `parenthesis)
            (comma-separator-parser))))
      (parser (reverse $line-stack))))

  (define (comma-separator-parser)
    (lets
      ((parser _) (comma-parser))
      ((parser _) (space-parser))
      (parser #t)))

  (define (colon-separator-parser)
    (lets
      ((parser _) (colon-parser))
      ((parser _) (space-parser))
      (parser #t)))

  (define (parenthesized-parser $parser)
    (lets
      ((parser _) (exact-char-parser #\())
      ((parser $item) $parser)
      ((parser _) (exact-char-parser #\)))
      (parser $item)))
)
