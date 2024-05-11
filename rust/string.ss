(library (rust string)
  (export
    rust-expr-string
    rust-body-string
    mut get set
    u8 u8+ u8+1 u8- u8-1)
  (import (micascheme) (code) (rust code))
  (export (import (micascheme)) let)

  (define-aux-keywords
    mut get set
    u8 u8+ u8+1 u8- u8-1)

  (data (body decls locals))

  (define (rust-body-string $syntax)
    (code-string
      (apply code-append
        (map-with
          ($decl (reverse (body-decls (rust-body (stack) $syntax))))
          (code $decl "\n")))))

  (define (rust-expr-string $syntax)
    (code-string (rust-expr-code (stack) $syntax)))

  (define (rust-expr-code $locals $syntax)
    (syntax-case $syntax (get u8 u8+ u8+1 u8- u8-1)
      ((get $index)
        (list-ref $locals (datum $index)))
      ((u8 $number)
        (number-code (datum $number)))
      ((u8+ $lhs $rhs)
        (call-code
          (rust-expr-code $locals #'$lhs)
          "wrapping_add"
          (rust-expr-code $locals #'$rhs)))
      ((u8+1 $lhs)
        (call-code
          (rust-expr-code $locals #'$lhs)
          "wrapping_inc"))
      ((u8- $lhs $rhs)
        (call-code
          (rust-expr-code $locals #'$lhs)
          "wrapping_sub"
          (rust-expr-code $locals #'$rhs)))
      ((u8-1 $lhs)
        (call-code
          (rust-expr-code $locals #'$lhs)
          "wrapping_dec"))))

  (define (var-code $locals)
    (code "v" (number-code (length $locals))))

  (define (rust-body $locals $syntax)
    (fold-left
      rust-body-push
      (body (stack) $locals)
      (syntax->list $syntax)))

  (define (rust-body-push (body $decls $locals) $syntax)
    (syntax-case $syntax (let mut)
      ((let $expr)
        (lets
          ($var-code (var-code $locals))
          (body
            (push $decls (declaration-code (let-code $var-code (rust-expr-code $locals #'$expr))))
            (push $locals $var-code))))))
)
