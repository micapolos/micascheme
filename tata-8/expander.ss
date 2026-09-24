(library (tata-8 expander)
  (export
    ; typed-ref-of

    ; typed-code
    check-expands)
  (import
    (scheme)
    (data)
    (lets)
    (keyword)
    (syntax)
    (check)
    (code)
    (string)
    (procedure)
    (tata-8 type)
    (tata-8 typed))

  (data expander)

  (define (type-code $type)
    (type-switch $type
      ((integer-type? _) (code "integer"))
      ((text-type? _) (code "text"))
      ((image-type? _) (code "image"))
      ((drawing-type? _) (code "drawing"))))

  (define (expand-expression-of $expander $syntax $type)
    (lets
      ($typed (expand-expression $expander $syntax))
      (cond
        ((equal? (typed-type $typed) $type)
          (typed-ref $typed))
        (else
          (syntax-error $syntax
            (format "invalid type ~a, expected ~a, in"
              (code-string (type-code (typed-type $typed)))
              (code-string (type-code $type))))))))

  (define (expand-expression $expander $syntax)
    (syntax-case $syntax ()
      (i
        (integer? (datum i))
        (typed integer-type
          (code
            "Integer.Constant"
            (code-in-round-brackets
              (number-code (datum i))))))
      (s
        (string? (datum s))
        (typed text-type
          (code
            "Text.Constant"
            (code-in-round-brackets
              "\""
              (string-code (datum s))
              "\""))))
      ((+ x y)
        (free-keyword? +)
        (expand-apply-2 $expander integer-type "Integer" "ADD" #'x #'y))
      ((- x y)
        (free-keyword? -)
        (expand-apply-2 $expander integer-type "Integer" "SUB" #'x #'y))
      ((* x y)
        (free-keyword? *)
        (expand-apply-2 $expander integer-type "Integer" "MUL" #'x #'y))
      ((/ x y)
        (free-keyword? /)
        (expand-apply-2 $expander integer-type "Integer" "DIV" #'x #'y))
      ((image $name)
        (and
          (free-keyword? image)
          (string? (datum $name)))
        (typed image-type
          (code
            "Image.Resource"
            (code-in-round-brackets
              (code "\"" (string-code (datum $name)) "\"")))))
      (empty-drawing
        (free-keyword? empty-drawing)
        (typed drawing-type
          (code
            "Drawing.Empty")))
      ((filled-rectangle (position (x $x) (y $y)) (size (width $width) (height $height)))
        (and
          (free-keyword? filled-rectangle)
          (free-keyword? position)
          (free-keyword? size)
          (free-keyword? x)
          (free-keyword? y)
          (free-keyword? width)
          (free-keyword? height))
        (typed drawing-type
          (code
            "Drawing.Rect"
            (code-in-round-brackets
              (indented-code #\newline
                (separated-code ",\n"
                  (expand-expression-of $expander #'$x integer-type)
                  (expand-expression-of $expander #'$y integer-type)
                  (expand-expression-of $expander #'$width integer-type)
                  (expand-expression-of $expander #'$height integer-type)))))))))

  (define (expand-apply-2 $expander $type $name $op $x $y)
    (typed $type
      (code
        (string-code $name)
        ".Apply2"
        (code-in-round-brackets
          (comma-separated-code
            (code (string-code $name) ".Op2." (string-code $op))
            (expand-expression-of $expander $x $type)
            (expand-expression-of $expander $y $type))))))

  (define-rule-syntax (check-expands in lines ...)
    (check
      (string=?
        (code-string (code (typed-ref (expand-expression expander #'in)) "\n"))
        (lines-string lines ...))))

  (define-rule-syntax (expand-program x)
    (code-string
      (code
        (newline-separated-code
          "package micapolos.zexy.examples"
          "import micapolos.zexy.*"
          "fun main() {"
          (indented-code (string-code (expand--string x)))
          "}"))))
)
