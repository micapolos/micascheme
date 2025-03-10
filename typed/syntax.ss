(library (typed syntax)
  (export
    syntax->typed)
  (import
    (micascheme)
    (any)
    (syntax lookup)
    (evaluator)
    (typed type)
    (typed typed)
    (typed phased)
    (typed keywords))

  (define (syntax->typed $phase $lookup $syntax)
    (syntax-case $syntax (assume type typeof any-boolean any-string any-number any-syntax any-lambda syntax lambda)
      ((assume t x)
        (lets
          ($type (syntax->type $phase $lookup #'t))
          (typed $type
            (if (zero? $phase) #'x
              (syntax-error $syntax "must be phase 0")))))
      ((type x)
        (lets
          ($type (syntax->type $phase $lookup #'x))
          (typed any-type
            (if (zero? $phase) (type->syntax $type) $type))))
      ((typeof x)
        (lets
          ($type (typed-type (syntax->typed $phase $lookup #'x)))
          (typed any-type
            (if (zero? $phase) (type->syntax $type) $type))))
      (any-boolean
        (typed (any any-boolean)
          (if (zero? $phase) #'any-boolean any-boolean)))
      (any-string
        (typed (any any-string)
          (if (zero? $phase) #'any-string any-string)))
      (any-number
        (typed (any any-number)
          (if (zero? $phase) #'any-number any-number)))
      (any-syntax
        (typed (any any-syntax)
          (if (zero? $phase) #'any-syntax any-syntax)))
      ((any-lambda (param ...) result)
        (typed any-any-lambda
          (lets
            ($any-lambda
              (make-any-lambda
                (map (partial syntax->type $phase $lookup) (syntaxes param ...))
                (syntax->type $phase $lookup #'result)))
            (if (zero? $phase) (type->syntax $any-lambda) $any-lambda))))
      (x
        (and (identifier? #'x) ($lookup #'x))
        (lets
          ($phased ($lookup #'x))
          (if (= (phased-phase $phased) $phase)
            (phased-value $phased)
            (syntax-error $syntax
              (format
                "invalid (phase ~s), expected (phase ~s), in"
                (phased-phase $phased)
                $phase)))))
      (x
        (boolean? (datum x))
        (typed any-boolean
          (if (zero? $phase) #'x (datum x))))
      (x
        (string? (datum x))
        (typed any-string
          (if (zero? $phase) #'x (datum x))))
      (x
        (number? (datum x))
        (typed any-number
          (if (zero? $phase) #'x (datum x))))
      ((syntax x)
        (typed any-syntax
          (if (zero? $phase) #'#'x #'x)))
      ((lambda (param ...) body)
        (lets
          ($typed-params
            (map
              (partial param-syntax->typed $phase $lookup)
              (syntaxes param ...)))
          ($param-types (map typed-type $typed-params))
          ($param-identifiers (map typed-value $typed-params))
          ($typed-body
            (syntax->typed $phase
              (fold-left (partial lookup+typed $phase) $lookup $typed-params)
              #'body))
          (typed
            (make-any-lambda $param-types (typed-type $typed-body))
            (if (zero? $phase)
              #`(lambda
                (#,@(map typed-value $typed-params))
                #,(typed-value $typed-body))
              (todo)))))
        ((target arg ...)
          (lets
            ($typed-lambda (syntax->typed-lambda $phase $lookup #'target))
            ($any-lambda (typed-type $typed-lambda))
            ($param-types (any-lambda-params $any-lambda))
            ($args (syntaxes arg ...))
            (run
              (when
                (not (= (length $param-types) (length $args)))
                (syntax-error $syntax
                  (format
                    "invalid number of args, expected ~s in"
                    (length $param-types)))))
            ($arg-values
              (map
                (partial syntax->typed-value $phase $lookup)
                $param-types
                $args))
            (typed
              (any-lambda-result $any-lambda)
              (if (zero? $phase)
                #`(
                  #,(typed-value $typed-lambda)
                  #,@$arg-values)
                (todo)))))))

  (define (syntax->type $phase $lookup $syntax)
    (typed-value (syntax->typed (+ $phase 1) $lookup $syntax)))

  (define (syntax->typed-value $phase $lookup $expected-type $syntax)
    (lets
      ($typed (syntax->typed $phase $lookup $syntax))
      ($type (typed-type $typed))
      (cond
        ((type=? $type $expected-type)
          (typed-value $typed))
        (else
          (syntax-error $syntax
            (format
              "invalid type ~s, expected ~s in"
              $type
              $expected-type))))))

  (define (syntax->typed-lambda $phase $lookup $syntax)
    (lets
      ($typed (syntax->typed $phase $lookup $syntax))
      (switch (typed-type $typed)
        ((any-lambda? $any-lambda)
          (typed $any-lambda (typed-value $typed)))
        ((else $type)
          (syntax-error $syntax
            (format
              "invalid type ~s, expected any-lambda in"
              $type))))))

  ; (define (syntax->typed $lookup $syntax)
  ;   (syntax-case $syntax (assume type)
  ;     ((assume t expr)
  ;       (typed
  ;         (syntax->type $lookup #'t)
  ;         (dynamic #'expr)))
  ;     (x
  ;       (identifier? #'x)
  ;       (lookup-ref $lookup #'x))
  ;     (x
  ;       (boolean? (datum x))
  ;       (typed any-boolean #'x))
  ;     (x
  ;       (char? (datum x))
  ;       (typed any-char #'x))
  ;     (x
  ;       (string? (datum x))
  ;       (typed any-string #'x))
  ;     (x
  ;       (fixnum? (datum x))
  ;       (typed any-fixnum #'x))
  ;     (x
  ;       (flonum? (datum x))
  ;       (typed any-flonum #'x))
  ;     ((lambda (param ...) body)
  ;       (lets
  ;         ($typed-params
  ;           (map
  ;             (partial param-syntax->typed $type-eval)
  ;             (syntaxes param ...)))
  ;         ($param-types (map typed-type $typed-params))
  ;         ($param-identifiers (map typed-value $typed-params))
  ;         ($typed-body
  ;           (syntax->typed
  ;             $type-eval
  ;             (fold-left lookup+typed $type-lookup $typed-params)
  ;             #'body))
  ;         (typed
  ;           (make-any-lambda $param-types (typed-type $typed-body))
  ;           #`(lambda
  ;             (#,@(map typed-value $typed-params))
  ;             #,(typed-value $typed-body)))))
  ;     ((target args ...)
  ;       (lets
  ;         ($typed-target (syntax->typed $type-eval $type-lookup #'target))
  ;         ($typed-lambda (syntax->typed-lambda $typed-target))
  ;         ($any-lambda (typed-type $typed-lambda))
  ;         ($param-types (map typed-type $typed-args))
  ;         (run
  ;           (when
  ;             (not (= (length $param-types) (length $typed-args)))
  ;             (syntax-error $syntax
  ;               (format
  ;                 "invalid number of args, expected ~s in"
  ;                 (length $param-types)))))
  ;         ($typed-args
  ;           (map
  ;             (partial syntax->typed-value $lookup)
  ;             $param-types
  ;             (syntaxes args ...)))
  ;         (typed
  ;           (any-lambda-result $any-lambda)
  ;           #`(
  ;             #,(typed-value $typed-target)
  ;             #,@$typed-args))))))

  (define (lookup+typed $phase $lookup $typed)
    (lookup+ $lookup
      (typed-value $typed)
      (phased $phase
        (typed
          (typed-type $typed)
          (if (zero? $phase)
            (typed-value $typed)
            (syntax->datum (typed-value $typed)))))))

  (define (param-syntax->typed $phase $lookup $syntax)
    (syntax-case $syntax ()
      ((type identifier)
        (identifier? #'identifier)
        (typed
          (syntax->type $phase $lookup #'type)
          #'identifier))
      (else
        (syntax-error $syntax "invalid parameter"))))
)
