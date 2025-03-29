(library (typed-scheme lang)
  (export define-type type)
  (import
    (micascheme)
    (typed-scheme type)
    (typed-scheme type-syntax)
    (typed-scheme keywords))
  (export (import (typed-scheme keywords)))

  (define-syntax (type $syntax $lookup)
    (let ()
      (define (lang-syntax->type $lookup $scope $syntax)
        (syntax->type lang-syntax->type $lookup $scope $syntax))
      (syntax-case $syntax ()
        ((id x)
          (type->syntax
            (lambda ($value) (syntax-error $syntax "native"))
            #'id
            (lang-syntax->type $lookup (stack) #'x))))))

  (define-syntax (define-type $syntax)
    (syntax-case $syntax ()
      ((id name)
        (identifier? #'name)
        #`(define-syntax name
          (make-compile-time-value
            (type-definition
              #f
              (gensym)
              (symbol->string (datum name))
              0))))
      ((id (name param ...))
        (for-all identifier? (syntaxes name param ...))
        #`(define-syntax name
          (make-compile-time-value
            (type-definition
              #f
              (gensym)
              (symbol->string (datum name))
              (length (syntaxes param ...))))))))
)
