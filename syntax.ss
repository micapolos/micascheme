(library (syntax)
  (export
    syntax-null?
    define-syntax-rule
    define-syntax-case
    define-aux-keyword
    define-aux-keywords
    expand-begin-syntaxes
    define-namespace
    syntax-map-identifiers)
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

  (define-syntax-rule (define-aux-keywords aux ...)
    (begin (define-aux-keyword aux) ...))

  (define-syntax define-namespace
    (lambda ($syntax)
      (syntax-case $syntax ()
        ((_ $name) (identifier? #'$name)
          (let
            (($define-identifier
              (datum->syntax #'$name
                (string->symbol
                  (string-append "define-" (symbol->string (datum $name)))))))

            #`(begin
              (define-syntax-rule (#,$define-identifier $name $value)
                (begin
                  (define-aux-keyword $name)
                  (define-property $name define-namespace (syntax $value))))

              (define-syntax $name
                (lambda ($syntax)
                  (lambda ($lookup)
                    (syntax-case $syntax ()
                      ((_ $id)
                        (and
                          (identifier? #'$id)
                          ($lookup #'$id #'define-namespace))
                        ($lookup #'$id #'define-namespace))))))))))))

  (define (syntax-map-identifiers $fn $syntax)
    (depth-syntax-map-identifiers $fn 0 $syntax))

  (define (depth-syntax-map-identifiers $fn $depth $syntax)
    (syntax-case $syntax ()
      (($syntax $body ...)
        (and
          (identifier? #'$syntax)
          (free-identifier=? #'$syntax #'syntax)
          (zero? $depth))
        #`($syntax $body ...))
      (($syntax $body ...)
        (and
          (identifier? #'$syntax)
          (free-identifier=? #'$syntax #'syntax)
          (< $depth 0))
        #`($syntax
          #,@(map
            (lambda ($body)
              (depth-syntax-map-identifiers $fn (add1 $depth) $body))
            (syntax->list #'($body ...)))))
      (($quasisyntax $body ...)
        (and
          (identifier? #'$quasisyntax)
          (free-identifier=? #'$quasisyntax #'quasisyntax))
        #`(
          $quasisyntax
          #,@(map
            (lambda ($body)
              (depth-syntax-map-identifiers $fn (add1 $depth) $body))
            (syntax->list #'($body ...)))))
      (($unsyntax $body ...)
        (and
          (identifier? #'$unsyntax)
          (free-identifier=? #'$unsyntax #'unsyntax))
        #`(
          $unsyntax
          #,@(map
            (lambda ($body)
              (depth-syntax-map-identifiers $fn (sub1 $depth) $body))
            (syntax->list #'($body ...)))))
      (($unsyntax-splicing $body ...)
        (and
          (identifier? #'$unsyntax-splicing)
          (free-identifier=? #'$unsyntax-splicing #'unsyntax-splicing))
        #`(
          $unsyntax-splicing
          #,@(map
            (lambda ($body)
              (depth-syntax-map-identifiers $fn (sub1 $depth) $body))
            (syntax->list #'($body ...)))))
      (($head . $tail)
        (depth-inner-syntax-map-identifiers $fn $depth $syntax))
      ($id
        (and (zero? $depth) (identifier? #'$id))
        ($fn #'$id))
      ($other #'$other)))

  (define (depth-inner-syntax-map-identifiers $fn $depth $syntax)
    (syntax-case $syntax ()
      (() $syntax)
      (($head . $tail)
        #`(
          #,(depth-syntax-map-identifiers $fn $depth #'$head)
          .
          #,(depth-inner-syntax-map-identifiers $fn $depth #'$tail)))))
)
