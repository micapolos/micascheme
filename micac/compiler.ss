(library (micac compiler)
  (export
    compiler
    syntax->type
    compiler-syntax->body)
  (import (micascheme) (micac model) (micac syntax))

  (data (compiler lookup-fn))

  (define (syntax->type $syntax)
    (syntax-case $syntax (bool u8 u16 u32)
      (bool (type #'bool 1))
      (u8 (type #'u8 1))
      (u16 (type #'u16 2))
      (u32 (type #'u32 4))
      (_ (syntax-error $syntax "unknown type"))))

  (define (compiler-syntax->body $compiler $syntax)
    (fold-left
      (partial compiler-body+syntax $compiler)
      (body (list) (list) 0)
      (syntax->list $syntax)))

  (define (compiler-body-syntax->expr $compiler $body $syntax)
    (syntax-case $syntax ()
      (id (identifier? #'id))
      (_ (syntax-error $syntax "invalid expression"))))

  (define (compiler-body+syntax $compiler $body $syntax)
    (syntax-case $syntax (var)
      ((var type id)
        (lets
          ($type (syntax->type #'type))
          (body
            (push
              (body-variables $body)
              (variable #'id $type (body-size $body)))
            (body-syntaxes $body)
            (+ (body-size $body) (type-size $type)))))
      ($other (syntax-error #'$other))))
)
