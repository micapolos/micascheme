(library (syntax)
  (export
    syntax-null?
    define-syntax-rule
    define-syntax-case
    define-aux-keyword
    expand-begin-syntaxes)
  (import (scheme))

  (define (syntax-null? $syntax)
    (null? (syntax->datum $syntax)))

  (define (expand-begin-syntaxes $syntaxes)
    (apply append
      (map
        (lambda ($syntax)
          (syntax-case $syntax (begin)
            ((begin $syntax ...)
              (syntax->list #'($syntax ...)))
            ($other (list #'other))))
        $syntaxes)))

  (define-syntax define-syntax-rule
    (syntax-rules ()
      ((_ (name param ...) body)
        (define-syntax name
          (syntax-rules ()
            ((_ param ...) body))))
      ((_ name body)
        (define-syntax name
          (lambda ($syntax)
            (syntax-case $syntax ()
              (_ #`body)))))))

  (define-syntax define-syntax-case
    (syntax-rules ()
      ((_ ($name $param ...) $body)
        (define-syntax-case ($name $param ...) () $body))
      ((_ ($name $param ...) $keywords $body)
        (define-syntax-case $name $keywords
          ((_ $param ...) $body)))
      ((_ $name $keywords $case ...)
        (define-syntax $name
          (lambda ($tmp)
            (syntax-case $tmp $keywords
              $case ...))))))

  (define-syntax-rule (define-aux-keyword aux)
    (define-syntax-rule aux
      (syntax-error (quote aux) "misplaced aux keyword")))
)
