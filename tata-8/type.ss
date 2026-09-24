(library (tata-8 type)
  (export
    integer-type
    integer-type?

    drawing-type
    drawing-type?

    type?

    typed
    typed?
    typed-type
    typed-ref
    typed-ref-of

    typed-code

    filled-rectangle
    empty-drawing

    tata-string
    tata-program-string
    check-tata)
  (import
    (scheme)
    (code)
    (data)
    (union)
    (keyword)
    (procedure)
    (lets)
    (check)
    (syntax))

  (data integer-type)
  (data drawing-type)
  (union (type integer-type drawing-type))

  (data (typed type ref))

  (define-syntax (typed-ref-of $syntax)
    (syntax-case $syntax ()
      ((_ x type)
        #`(lets
          ($type type)
          ($typed (typed-code x))
          (cond
            ((equal? (typed-type $typed) $type)
              (typed-ref $typed))
            (else
              (syntax-error #'x "invalid type")))))))

  (define-keywords filled-rectangle empty-drawing)

  (define-rule-syntax (tata-string x)
    (code-string (app (typed-ref (typed-code x)) '())))

  (define-syntax (typed-code $syntax)
    (syntax-case $syntax (empty-drawing filled-rectangle +)
      ((_ i)
        (integer? (datum i))
        #`(typed integer-type
          (lambda (env)
            (code
              "Integer.Constant"
              (code-in-round-brackets
                (number-code i))))))
      ((_ (+ x y))
        #`(typed integer-type
          (lambda (env)
            (code
              "Integer.Apply2"
              (code-in-round-brackets
                (comma-separated-code
                  "Integer.Op2.ADD"
                  (app (typed-ref-of x integer-type) env)
                  (app (typed-ref-of y integer-type) env)))))))
      ((_ empty-drawing)
        #`(typed drawing-type
          (lambda (env)
            (code
              "Drawing.Empty"))))
      ((_ (filled-rectangle x y width height))
        #`(typed drawing-type
          (lambda (env)
            (code
              "Drawing.Rect"
              (code-in-round-brackets
                (comma-separated-code
                  (app (typed-ref-of x integer-type) env)
                  (app (typed-ref-of y integer-type) env)
                  (app (typed-ref-of width integer-type) env)
                  (app (typed-ref-of height integer-type) env)))))))))

  (define-rule-syntax (check-tata in out)
    (check (string=? (tata-string in) out)))

  (define-rule-syntax (tata-program-string x)
    (code-string
      (code
        (newline-separated-code
          "package micapolos.zexy.examples"
          "import micapolos.zexy.*"
          "fun main() {"
          (indented-code (string-code (tata-string x)))
          "}"))))
)
