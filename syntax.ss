(library (syntax)
  (export
    identifiers?
    syntax-null?
    define-rule-syntax
    define-case-syntax
    define-aux-keyword
    define-aux-keywords
    expand-begin-syntaxes
    define-namespace
    define-lookup-syntax
    syntax-selector
    syntax-pattern-id
    syntax-rule-id
    syntax-case-opt
    inline)
  (import (scheme))

  (define (identifiers? $syntax)
    (for-all identifier? (syntax->list $syntax)))

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

  (define-syntax define-rule-syntax
    (syntax-rules ()
      ((_ (name param ...) body)
        (define-syntax name
          (syntax-rules ()
            ((_ param ...) body))))
      ((_ name body)
        (define-syntax (name $syntax)
          (syntax-case $syntax ()
            (_ #`body))))))

  (define-syntax define-case-syntax
    (syntax-rules ()
      ((_ ($name $param ...) $body)
        (define-case-syntax ($name $param ...) () $body))
      ((_ ($name $param ...) $keywords $body)
        (define-case-syntax $name $keywords
          ((_ $param ...) $body)))
      ((_ $name $keywords $case ...)
        (define-syntax $name
          (lambda ($tmp)
            (syntax-case $tmp $keywords
              $case ...))))))

  (define-rule-syntax (define-aux-keyword aux)
    (define-rule-syntax aux
      (syntax-error (quote aux) "misplaced aux keyword")))

  (define-rule-syntax (define-aux-keywords aux ...)
    (begin (define-aux-keyword aux) ...))

  (define-syntax (define-namespace $syntax)
    (syntax-case $syntax ()
      ((_ $name) (identifier? #'$name)
        (let
          (($define-identifier
            (datum->syntax #'$name
              (string->symbol
                (string-append "define-" (symbol->string (datum $name)))))))

          #`(begin
            (define-rule-syntax (#,$define-identifier $name $value)
              (begin
                (define-aux-keyword $name)
                (define-property $name define-namespace (syntax $value))))

            (define-lookup-syntax ($name $syntax $lookup)
              (syntax-case $syntax ()
                ((_ $id)
                  (and
                    (identifier? #'$id)
                    ($lookup #'$id #'define-namespace))
                  ($lookup #'$id #'define-namespace)))))))))

  (define-syntax define-lookup-syntax
    (syntax-rules ()
      ((_ $name $body)
        (identifier? #'$name)
        (define-syntax $name $body))
      ((_ ($name $syntax) $body)
        (and (identifier? #'$name) (identifier? #'$syntax))
        (define-syntax ($name $syntax) $body))
      ((_ ($name $syntax $lookup) $body)
        (and (identifier? #'$name) (identifier? #'$syntax) (identifier? #'$lookup))
        (define-syntax ($name $syntax)
          (lambda ($lookup) $body)))))

  (define (syntax-selector $syntax)
    (syntax-case-opt $syntax ()
      ($id (identifier? #'$id) #'$id)
      (($id $arg ...) (identifier? #'$id) #'$id)))

  (define (syntax-pattern-id $pattern)
    (or
      (syntax-selector $pattern)
      (syntax-error $pattern "pattern syntax error")))

  (define (syntax-rule-id $rule)
    (syntax-case $rule ()
      (($pattern $rest ...)
        (syntax-pattern-id #'$pattern))))

  (define-syntax (syntax-case-opt $syntax)
    (syntax-case $syntax ()
      ((_ $expr ($literal ...) $clause ...)
        #`(syntax-case $expr ($literal ...)
          $clause ... (_ #f)))))

  (define-syntax (inline $syntax)
    (syntax-case $syntax ()
      ((_ $expr)
        #'(let-syntax
          ((inlined (lambda (_) $expr)))
          inlined))))
)
