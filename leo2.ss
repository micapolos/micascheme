(library (leo2)
  (export
    binding binding? binding-identifier binding-type binding-syntax binding-value
    term term? term-types term-syntax term-value
    leo leo? leo-bindings leo-terms leo-parent-option

    syntax-list-leo)
  (import (micascheme))

  (data (typed value type))

  (data (term types syntax values))
  (data (binding term identifiers))
  (data (leo bindings terms parent-option))

  (data (variable index))
  (data (abstraction params results))
  (data (application fn args))
  (data (struct name values))
  (data (enum name values))

  (data (any-string))
  (data (any-number))

  (define empty-leo (leo (list) (list) #f))

  (define (syntax-list-leo $syntax-list)
    (fold-left leo+syntax empty-leo $syntax-list))

  (define (leo-syntax-term $leo $syntax)
    (syntax-case (syntax-normalize $syntax) (lambda)
      ((lambda ($param ...) $body ...)
        (syntax-error $syntax "todo lambda"))
      (($identifier $arg ...) (identifier? #`$identifier)
        (syntax-error $syntax "todo field"))
      ($other 
        (switch (syntax->datum #`$other)
          ((string? $string) (term (list (any-string)) $syntax (list $string)))
          ((number? $number) (term (list (any-number)) $syntax (list $number)))
          ((else $other) (syntax-error $syntax))))))

  (define (leo+syntaxes $leo $syntaxes)
    (fold-left leo+syntax $leo $syntaxes))

  (define (leo+syntax $leo $syntax)
    (leo+term
      $leo
      (leo-syntax-term $leo $syntax)))

  (define (leo+term $leo $term)
    (leo
      (leo-bindings $leo)
      (push (leo-terms $leo) $term)
      (leo-parent-option $leo)))

  (define (syntax-normalize $syntax)
    (syntax-case $syntax ()
      ($identifier (identifier? #`$identifier) #`($identifier))
      ($other #`$other)))
)