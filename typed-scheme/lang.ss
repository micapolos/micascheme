(library (typed-scheme lang)
  (export
    define-type
    get-type-definition
    type
    assume-type
    typeof)
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

  (define-syntax (get-type-definition $syntax $lookup)
    (syntax-case $syntax ()
      ((id name)
        (and (identifier? #'name) (type-definition? ($lookup #'name)))
        (type-definition->syntax #'id ($lookup #'name)))))

  (define-rule-syntax (assume-type id t)
    (define-property id type (type t)))

  (define-syntax (typeof $syntax $lookup)
    (syntax-case $syntax ()
      ((typeof id)
        (identifier? #'id)
          (lets
            ($type? ($lookup #'id #'type))
            (if $type?
              (type->syntax
                (lambda ($value) (syntax-error $syntax "native"))
                #'typeof
                $type?)
              (syntax-error #'id "unknown type"))))))
)
