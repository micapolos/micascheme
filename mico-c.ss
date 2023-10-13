(library (mico-c)
  (export
    typed typed? typed-term typed-type
    arrow arrow? arrow-params arrow-body
    syntax->typed-c
    syntax->type)
  (import (micascheme))

  (data (context typed-bindings type-bindings))
  (data (typed term type))

  (data (arrow params body))

  (define (empty-context) (context (stack) (stack)))

  (define (syntax->type $syntax)
    (context-syntax->type (empty-context) $syntax))

  (define (syntax->typed-c $syntax)
    (context-syntax->typed-c (empty-context) $syntax))

  (define (type-apply-opt $type $args)
    (and
      (arrow? $type)
      (for-all equal? (arrow-params $type) $args)
      (arrow-body $type)))

  (define (context-syntax->type $context $syntax)
    (syntax-case $syntax ()
      (($function ($param ...) $body)
        (identifier-named? (syntax $function) function)
        (arrow
          (map (partial context-syntax->type $context) (syntax->list (syntax ($param ...))))
          (context-syntax->type $context (syntax $body))))
      ($name
        (identifier? (syntax $name))
        (syntax->datum (syntax $name)))
      (else (syntax-error $syntax "invalid type"))))

  (define (context-syntax->typed-c $context $syntax)
    (syntax-case $syntax ()
      (($c $string $type)
        (and (identifier-named? (syntax $c) c))
          (typed
            (syntax->datum (syntax $string))
            (context-syntax->type $context (syntax $type))))
      (($fn $arg ...)
        (lets
          ($typed-fn (context-syntax->typed-c $context (syntax $fn)))
          ($typed-args (map (partial context-syntax->typed-c $context) (syntax->list (syntax ($arg ...)))))
          ($body-type
            (or
              (type-apply-opt (typed-type $typed-fn) (map typed-type $typed-args))
              (syntax-error $syntax "type mismatch")))
          (typed
            (string-append
              (typed-term $typed-fn)
              "("
              (apply string-append (intercalate (map typed-term $typed-args) ", "))
              ")")
            $body-type)))
      ($literal
        (switch (syntax->datum (syntax $literal))
          ((integer? $integer) (typed (number->string $integer) `int))
          ((else $other) (syntax-error $syntax "not typed"))))
      (else
        (syntax-error $syntax "not typed"))))
)
