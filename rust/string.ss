(library (rust string)
  (export
    rust-expr-string
    rust-block-string
    rust
    in get then
    u8 u8+ u8+1 u8- u8-1)
  (import (micascheme) (code) (rust code))
  (export (import (micascheme)) let)

  (define-aux-keywords
    rust
    in get then
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
    (syntax-case $syntax (rust get u8 u8+ u8+1 u8- u8-1)
      ((rust $string)
        (string-code (datum $string)))
      ((get $index)
        (list-ref $locals (datum $index)))
      ((u8 $number)
        (parens-code (code (number-code (datum $number)) " as u8")))
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
    (syntax-case $syntax (let in then)
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
          ((block $in-decls $in-exports $in-next-var-index)
            (rust-block
              (push-all $locals $exports)
              $next-var-index
              #'($body ...)))
          (block
            (push-all $decls $in-decls)
            $in-exports
            $in-next-var-index)))
      ((then $body ...)
        (lets
          ((block $then-decls $then-exports $then-next-var-index)
            (rust-block
              (push-all $locals $exports)
              $next-var-index
              #'($body ...)))
          (block
            (push-all $decls $then-decls)
            (push-all $exports $then-exports)
            $then-next-var-index)))))
)
