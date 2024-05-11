(library (rust string)
  (export
    rust-expr-string
    rust-block-string
    in get
    u8 u8+ u8+1 u8- u8-1)
  (import (micascheme) (code) (rust code))
  (export (import (micascheme)) let)

  (define-aux-keywords
    in get
    u8 u8+ u8+1 u8- u8-1)

  (data (block decls exports next-var-index))

  (define (rust-block-string $syntax)
    (code-string
      (apply code-append
        (map-with
          ($decl (reverse (block-decls (rust-block (stack) 0 $syntax))))
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

  (define (var-code $index)
    (code "v" (number-code $index)))

  (define (rust-block $locals $next-var-index $syntax)
    (fold-left
      (partial rust-block-push $locals)
      (block (stack) (stack) $next-var-index)
      (syntax->list $syntax)))

  (define (rust-block-push $locals (block $decls $exports $next-var-index) $syntax)
    (syntax-case $syntax (let in)
      ((let $expr)
        (lets
          ($var-code (var-code $next-var-index))
          ($expr-code (rust-expr-code $locals #'$expr))
          (block
            (push $decls (declaration-code (let-code $var-code $expr-code)))
            (push $exports $var-code)
            (add1 $next-var-index))))
      ((in $body ...)
        (lets
          ($in-block
            (rust-block
              (push-all $locals $exports)
              $next-var-index
              #'($body ...)))
          (block
            (push-all $decls (block-decls $in-block))
            (block-exports $in-block)
            (block-next-var-index $in-block))))))
)
