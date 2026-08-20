(library (tt lang)
  (export
    define-class
    define-macro
    define-prim
    fails
    print-typeof
    (rename
      (%define define)
      (%let let)
      (%lets lets)
      (%define-syntax define-syntax)
      (%check check)
      (%print print)))
  (import
    (scheme)
    (syntax)
    (syntaxes)
    (lets)
    (procedure)
    (check)
    (switch)
    (keyword)
    (tt term)
    (tt primitive)
    (tt compiler)
    (tt lang-macros)
    (prefix (only (scheme) not) %)
    (prefix (tt keywords) %))
  (export (import (tt keywords)))

  (define-keywords fails)

  (define-syntax (define-class $syntax)
    (compile-define-class $syntax))

  (define-syntax (define-macro $syntax)
    (compile-define-macro $syntax))

  (define-syntax (%define-syntax $syntax)
    (compile-define-syntax $syntax))

  (define-syntax (%check $syntax)
    (lambda ($lookup)
      (syntax-case $syntax (fails)
        ((_ (fails x ...))
          (for-all
            (lambda ($x)
              (check
                (raises
                  (compile-typed $lookup $x))))
            #'(x ...)))
        ((_ (not (pred? a)))
          (free-keyword? not)
          (lets
            ((typed $type $a) (compile-typed $lookup #'a))
            ($pred? (compile-value $lookup (arrow (list $type) #f boolean-type) #'pred?))
            #`(check (%not (#,$pred? #,$a)))))
        ((_ (not (eq? a b)))
          (free-keyword? not)
          (lets
            ($typed-a (compile-typed $lookup #'a))
            ($type (typed-type $typed-a))
            ($a (typed-ref $typed-a))
            ($b (compile-value $lookup $type #'b))
            ($eq? (compile-value $lookup (arrow (list $type $type) #f boolean-type) #'eq?))
            #`(check (%not (#,$eq? #,$a #,$b)))))
        ((_ (pred? a))
          (lets
            ((typed $type $a) (compile-typed $lookup #'a))
            ($pred? (compile-value $lookup (arrow (list $type) #f boolean-type) #'pred?))
            #`(check (#,$pred? #,$a))))
        ((_ (eq? a b))
          (lets
            ($typed-a (compile-typed $lookup #'a))
            ($type (typed-type $typed-a))
            ($a (typed-ref $typed-a))
            ($b (compile-value $lookup $type #'b))
            ($eq? (compile-value $lookup (arrow (list $type $type) #f boolean-type) #'eq?))
            #`(check (#,$eq? #,$a #,$b))))
        ((_ x d)
          (lets
            ($typed (compile-typed $lookup #'x))
            #`(check
              (equal?
                `(typed
                  #,(literal->syntax
                    (term->datum primitive->datum 0 (typed-type $typed)))
                  ,#,(typed-ref $typed))
                'd)))))))

  (define-syntax (%define $syntax)
    (lambda ($lookup)
      (compile-define $lookup $syntax)))

  (define-syntax (define-prim $syntax)
    (lambda ($lookup)
      (compile-define-prim $lookup $syntax)))

  (define-syntax (%print $syntax)
    (lambda ($lookup)
      (compile-print $lookup $syntax)))

  (define-syntax (print-typeof $syntax)
    (lambda ($lookup)
      (compile-print-typeof $lookup $syntax)))

  (%define-syntax %let
    (syntax-rules ()
      ((_ ((id x) ...) body)
        ((%lambda ((id (%typeof x)) ...) body) x ...))
      ((_ (rec-id rec-t) ((id x) ...) body)
        ((%lambda (rec-id rec-t) ((id (%typeof x)) ...) body) x ...))))

  (%define-syntax %lets
    (syntax-rules ()
      ((_ body) body)
      ((_ (id x) xs ... body)
        (%let ((id x)) (%lets xs ... body)))))
)
