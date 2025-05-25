(library (sjasm expr)
  (export sjasm-expr?)
  (import (micascheme))

  (define (sjasm-expr? $identifier-sjasm? $syntax)
    (syntax-case? $syntax ()
      (x (char? (datum x)) #'x)
      (x (string? (datum x)) #'x)
      (x (number? (datum x)) #'x)
      ((id arg ...)
        (and
          (identifier? #'id)
          ($identifier-sjasm? #'id))
        (lets
          ($args? (?list->list? (map (partial sjasm-expr? $identifier-sjasm?) #'(arg ...))))
          (and $args? #`(id #,@$args?))))))
)
