(library (typed-scheme lang)
  (export
    define-type
    assume-type
    typed
    (rename (define-typed define)))
  (import
    (micascheme)
    (typed-scheme type)
    (typed-scheme types)
    (typed-scheme type-syntax)
    (typed-scheme expr-syntax)
    (typed-scheme expr)
    (typed-scheme keywords))
  (export
    (import (typed-scheme keywords))
    (import (only (micascheme) lambda null)))

  (meta define (lang-syntax->type $type-definition-lookup $scope $syntax)
    (syntax->type lang-syntax->type $type-definition-lookup $scope $syntax))

  (meta define (lang-syntax->expr $type-definition-lookup $type-lookup $type-scope $scope $syntax)
    (syntax-case $syntax (null)
      (null
        (expr null-type (native-term #''())))
      (c
        (char? (datum c))
        (expr char-type (native-term #'c)))
      (s
        (string? (datum s))
        (expr string-type (native-term #'s)))
      (n
        (number? (datum n))
        (expr number-type (native-term #'n)))
      (b
        (boolean? (datum b))
        (expr boolean-type (native-term #'b)))
      (other
        (syntax->expr
          lang-syntax->type
          lang-syntax->expr
          $type-definition-lookup
          $type-lookup
          $type-scope
          $scope
          #'other))))

  (meta define (type-definition-lookup $lookup) $lookup)

  (meta define (type-lookup $lookup)
    (lambda ($id)
      ($lookup $id #'type)))

  (define-syntax (type $syntax)
    (syntax-error $syntax))

  (define-syntax (typed $syntax $lookup)
    (syntax-case $syntax ()
      ((typed x)
        (fluent
          (lang-syntax->expr
            (type-definition-lookup $lookup)
            (type-lookup $lookup)
            (stack)
            (stack)
            #'x)
          (let $expr (expr->syntax #'typed identity (stack) $expr))))))

  (define-syntax (define-typed $syntax $lookup)
    (syntax-case $syntax ()
      ((define-typed name value)
        (identifier? #'name)
        (lets
          ($expr
            (lang-syntax->expr
              (type-definition-lookup $lookup)
              (type-lookup $lookup)
              (stack)
              (stack)
              #'value))
          #`(begin
            (define name
              #,(expr->syntax #'define-typed identity (stack) $expr))
            (define-property name type
              #,(type->syntax
                (lambda ($value) (syntax-error $syntax "native"))
                #'define-typed
                (expr-type $expr))))))
      ((define-typed (name param ...) value)
        #`(define-typed name
          (lambda (param ...) value)))))

  (define-syntax (define-type $syntax)
    (syntax-case $syntax ()
      ((id name definition)
        (identifier? #'name)
        #`(define-syntax name
          (make-compile-time-value definition)))
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

  (define-syntax (assume-type $syntax $lookup)
    (syntax-case $syntax ()
      ((assume-type id t)
        (identifier? #'id)
        #`(define-property id type
          #,(type->syntax
            (lambda ($value) (syntax-error #'id))
            #'assume-type
            (lang-syntax->type
              (type-definition-lookup $lookup)
              (stack)
              #'t))))
      ((assume-type (id param ...) result)
        #'(assume-type id (any-lambda (param ...) result)))))
)
