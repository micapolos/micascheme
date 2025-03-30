(library (typed-scheme lang)
  (export
    define-type
    get-type-definition
    type
    assume-type
    typeof
    typed
    define-typed)
  (import
    (micascheme)
    (typed-scheme type)
    (typed-scheme types)
    (typed-scheme type-syntax)
    (typed-scheme expr-syntax)
    (typed-scheme expr)
    (typed-scheme keywords))
  (export (import (typed-scheme keywords)))

  (meta define (lang-syntax->type $lookup $scope $syntax)
    (syntax->type lang-syntax->type $lookup $scope $syntax))

  (meta define (lang-syntax->expr $type-lookup $type-scope $scope $syntax)
    (syntax-case $syntax ()
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
        (syntax->expr lang-syntax->type lang-syntax->expr $type-lookup $type-scope $scope #'other))))

  (meta define (type-definition-lookup $lookup)
    $lookup)

  (meta define (type-lookup $lookup)
    (lambda ($id)
      ($lookup $id #'type)))

  (define-syntax (typed $syntax $lookup)
    (syntax-case $syntax ()
      ((typed x)
        (fluent
          (lang-syntax->expr
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
          ($expr (lang-syntax->expr (type-lookup $lookup) (stack) (stack) #'value))
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

  (define-syntax (type $syntax $lookup)
    (syntax-case $syntax ()
      ((id x)
        (type->syntax
          (lambda ($value) (syntax-error $syntax "native"))
          #'id
          (lang-syntax->type (type-definition-lookup $lookup) (stack) #'x)))))

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

  (define-syntax (get-type-definition $syntax $lookup)
    (syntax-case $syntax ()
      ((id name)
        (and (identifier? #'name) (type-definition? ($lookup #'name)))
        (type-definition->syntax #'id ($lookup #'name)))))

  (define-rules-syntax
    ((assume-type id t)
      (identifier? #'id)
      (define-property id type (type t)))
    ((assume-type (id param ...) result)
      (assume-type id (a-lambda (param ...) result))))

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
