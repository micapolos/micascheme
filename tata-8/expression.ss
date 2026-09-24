(library (tata-8 expression)
  (export
    code-expression
    code-expression?
    code-expression-code

    symbolic-expression
    symbolic-expression?
    symbolic-expression-symbol
    symbolic-expression-args

    expression?
    expression-switch)
  (import
    (scheme)
    (data)
    (union))

  (data (code-expression code))
  (data (symbolic-expression symbol args))

  (union
    (expression
      code-expression
      symbolic-expression))
)
