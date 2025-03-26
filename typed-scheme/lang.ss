(library (typed-scheme lang)
  (export define-type type)
  (import
    (micascheme)
    (typed-scheme type)
    (typed-scheme type-syntax)
    (typed-scheme keywords))
  (export (import (typed-scheme keywords)))

  (define-syntax (define-type $syntax)
    (syntax-case $syntax ()
      ((id name)
        (lets
          ($name (datum->syntax #'id (symbol->string (syntax->datum (identifier name)))))
          #`(begin
            (define def (type-definition #f (gensym) #,$name 0))
            (define name (defined-type #f def (immutable-vector)))
            (define-property name type-definition? #t))))
      ((id (name param ...))
        (lets
          ($name (datum->syntax #'id (symbol->string (syntax->datum (identifier name)))))
          ($arity (length (syntaxes param ...)))
          ($arity-syntax (datum->syntax #'id $arity))
          #`(begin
            (define def (type-definition #f (gensym) #,$name #,$arity-syntax))
            (define (name param ...)
              (defined-type #f def (immutable-vector param ...)))
            (define-property name type-definition? #t))))))

  (define-syntax (type $syntax $lookup)
    (syntax-case $syntax ()
      ((_ t)
        (syntax->type
          (lambda ($identifier) ($lookup $identifier #'type-definition?))
          (stack)
          #'t))))
)
