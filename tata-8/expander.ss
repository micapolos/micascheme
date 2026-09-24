(library (tata-8 expander)
  (export
    ; typed-ref-of

    ; typed-code
    check-expands)
  (import
    (scheme)
    (lets)
    (keyword)
    (syntax)
    (check)
    (code)
    (string)
    (procedure)
    (tata-8 type)
    (tata-8 typed))

  (define (type-code $type)
    (type-switch $type
      ((integer-type? _) (code "integer"))
      ((text-type? _) (code "text"))
      ((drawing-type? _) (code "drawing"))))

  (define (expand-expression-of $syntax $type)
    (lets
      ($typed (expand-expression $syntax))
      (cond
        ((equal? (typed-type $typed) $type)
          (typed-ref $typed))
        (else
          (syntax-error $syntax
            (format "invalid type ~a, expected ~a, in"
              (code-string (type-code (typed-type $typed)))
              (code-string (type-code $type))))))))

  (define (expand-expression $syntax)
    (syntax-case $syntax ()
      (i
        (integer? (datum i))
        (typed integer-type
          (lambda (env)
            (code
              "Integer.Constant"
              (code-in-round-brackets
                (number-code (datum i)))))))
      (s
        (string? (datum s))
        (typed text-type
          (lambda (env)
            (code
              "Text.Constant"
              (code-in-round-brackets
                "\""
                (string-code (datum s))
                "\"")))))
      ((+ x y)
        (free-keyword? +)
        (typed integer-type
          (lambda (env)
            (code
              "Integer.Apply2"
              (code-in-round-brackets
                (comma-separated-code
                  "Integer.Op2.ADD"
                  (app (expand-expression-of #'x integer-type) env)
                  (app (expand-expression-of #'y integer-type) env)))))))
      (empty-drawing
        (free-keyword? empty-drawing)
        (typed drawing-type
          (lambda (env)
            (code
              "Drawing.Empty"))))
      ((filled-rectangle x y width height)
        (free-keyword? filled-rectangle)
        (typed drawing-type
          (lambda (env)
            (code
              "Drawing.Rect"
              (code-in-round-brackets
                (indented-code #\newline
                  (separated-code ",\n"
                    (app (expand-expression-of #'x integer-type) env)
                    (app (expand-expression-of #'y integer-type) env)
                    (app (expand-expression-of #'width integer-type) env)
                    (app (expand-expression-of #'height integer-type) env))))))))))

  (define-rule-syntax (check-expands in lines ...)
    (check
      (string=?
        (code-string (code (app (typed-ref (expand-expression #'in)) '()) "\n"))
        (lines-string lines ...))))

  ; (define-rule-syntax (tata-program-string x)
  ;   (code-string
  ;     (code
  ;       (newline-separated-code
  ;         "package micapolos.zexy.examples"
  ;         "import micapolos.zexy.*"
  ;         "fun main() {"
  ;         (indented-code (string-code (tata-string x)))
  ;         "}"))))

)
