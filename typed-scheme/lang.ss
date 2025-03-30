(library (typed-scheme lang)
  (export
    define-type
    get-type-definition
    type
    assume-type
    typeof
    typed)
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

  (define-syntax (typed $syntax $lookup)
    (syntax-case $syntax ()
      ((typed x)
        (fluent
          (lang-syntax->expr
            (lambda ($id) ($lookup $id #'type))
            (stack)
            (stack)
            #'x)
          (let $expr (expr->syntax #'typed identity (stack) $expr))))))

  (define-syntax (type $syntax $lookup)
    (syntax-case $syntax ()
      ((id x)
        (type->syntax
          (lambda ($value) (syntax-error $syntax "native"))
          #'id
          (lang-syntax->type $lookup (stack) #'x)))))

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
