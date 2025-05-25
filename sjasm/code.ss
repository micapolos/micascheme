(library (sjasm code)
  (export
    lines-code
    line-code
    instrs-code
    instr-code
    expr-code)
  (import (micascheme) (code) (sjasm keywords))

  (define (lines-code $syntaxes)
    (list->separated-code
      (code #\newline)
      (map line-code $syntaxes)))

  (define (line-code $syntax)
    (syntax-case $syntax (:)
      (id
        (identifier? #'id)
        (identifier-code #'id))
      ((id : x ...)
        (identifier? #'id)
        (space-separated-code
          (identifier-code #'id)
          (instr-code #'(x ...))))
      (instr (indented-code (instr-code #'instr)))))

  (define (instrs-code $syntaxes)
    (list->separated-code
      (code #\newline)
      (map instr-code $syntaxes)))

  (define (instr-code $syntax)
    (syntax-case $syntax (savenex)
      ((savenex op arg* ...)
        (space-separated-code
          (identifier-code #'savenex)
          (identifier-code #'op)
          (list->separated-code
            (code ", ")
            (map expr-code #'(arg* ...)))))
      ((op arg ...)
        (space-separated-code
          (identifier-code #'op)
          (list->separated-code
            (code ", ")
            (map expr-code #'(arg ...)))))))

  (define (expr-code $syntax)
    (syntax-case $syntax (+)
      (id
        (identifier? #'id)
        (identifier-code #'id))
      (id
        (number? (datum id))
        (number-code (datum id)))
      (id
        (char? (datum id))
        (number-code (char->integer (datum id))))
      (id
        (string? (datum id))
        (string-code (format "~s" (datum id))))
      ((expr)
        (code-in-round-brackets (expr-code #'expr)))
      ((+ a b)
        (space-separated-code
          (expr-code #'a)
          "+"
          (expr-code #'b)))))

  (define (identifier-code $syntax)
    (string-code (symbol->string (syntax->datum $syntax))))
)
