(library (rust string)
  (export
    rust-expr-string
    mut get set
    u8 u8+ u8+1 u8- u8-1)
  (import (micascheme) (code) (rust code))
  (export (import (micascheme)) let)

  (define-aux-keywords
    mut get set
    u8 u8+ u8+1 u8- u8-1)

  (define (rust-expr-string $syntax)
    (code-string (rust-expr-code (stack) $syntax)))

  (define (rust-expr-code $stack $syntax)
    (syntax-case $syntax (get u8 u8+ u8+1 u8- u8-1)
      ((get $index)
        (list-ref $stack (datum $index)))
      ((u8 $number)
        (number-code (datum $number)))
      ((u8+ $lhs $rhs)
        (call-code
          (rust-expr-code $stack #'$lhs)
          "wrapping_add"
          (rust-expr-code $stack #'$rhs)))
      ((u8+1 $lhs)
        (call-code
          (rust-expr-code $stack #'$lhs)
          "wrapping_inc"))
      ((u8- $lhs $rhs)
        (call-code
          (rust-expr-code $stack #'$lhs)
          "wrapping_sub"
          (rust-expr-code $stack #'$rhs)))
      ((u8-1 $lhs)
        (call-code
          (rust-expr-code $stack #'$lhs)
          "wrapping_dec"))))

  ; (define (var-code $stack)
  ;   (code "v" (number-code (length $stack))))

  ; (define (rust-body $stack $syntax)
  ;   (syntax-case $syntax ()
  ;     (() (stack))
  ;     (($decl $decl* ...)
  ;       (let-values
  ;         ((($body $stack) (rust-body-push $body $stack (car $syntax-list))))
  ;         (rust-body-push* $body $stack (cdr $syntax-list))))))

  ; (define (rust-body-push $body $stack $syntax)
  ;   (syntax-case $syntax (let mut)
  ;     ((let $expr)
  ;       (lets
  ;         ($var-code (var-code $stack))
  ;         (values
  ;           (push $body
  ;             (code "let " $var-code " = " (rust-expr-code $stack #'$expr)))
  ;           (push $stack $var-code))))))
)
