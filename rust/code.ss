(library (rust code)
  (export
    dot-code
    parens-code
    comma-separated-code
    invocation-code
    call-code
    let-code
    declaration-code)
  (import (scheme) (code) (list))

  (define (dot-code $lhs $rhs)
    (code $lhs "." $rhs))

  (define (parens-code $code)
    (code "(" $code ")"))

  (define (comma-separated-code . $args)
    (apply code-append (intercalate $args (code ", "))))

  (define (invocation-code . $args)
    (parens-code (apply code-append (intercalate $args (code ", ")))))

  (define (call-code $target $name . $args)
    (code-append
      (dot-code $target (string-code $name))
      (apply invocation-code $args)))

  (define (let-code $var $expr)
    (code "let " $var " = " $expr))

  (define (declaration-code $body)
    (code $body ";"))
)
