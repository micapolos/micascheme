(library (micalog core on-old-new)
  (export
    module->on-old-new-syntax
    statement->on-old-new-syntax
    statements->on-old-new-syntax)
  (import
    (micascheme)
    (prefix (micalog keywords) %))

  (define (module->on-old-new-syntax $module)
    (syntax-case $module (%module)
      ((%module name statement ...)
        #`(%module name
          #,@(unbegin-syntaxes
            (statements->on-old-new-syntax
              (syntaxes statement ...)))))))

  (define (statements->on-old-new-syntax $statements)
    (fluent $statements
      (map-using statement->on-old-new-syntax)
      (map-using unbegin-syntaxes)
      (flatten)
      (syntaxes->syntax)))

  (define (statement->on-old-new-syntax $statement)
    (syntax-case $statement (%cond %on)
      ((%cond clause ...)
        #`(%cond
          #,@(map clause->on-old-new-syntax
            (syntaxes clause ...))))
      ((%on (edge name) body ...)
        (lets
          ($old-name (generate-identifier (identifier-append #'name #'old- #'name)))
          #`(begin
            (%register 1 #,$old-name)
            (%on (edge #,$old-name name)
              #,@(unbegin-syntaxes
                (statements->on-old-new-syntax
                  (syntaxes body ...))))
            (%set 1 #,$old-name name))))
      (other #'other)))

  (define (clause->on-old-new-syntax $clause)
    (syntax-case $clause ()
      ((cond statement ...)
        #`(cond
          #,@(unbegin-syntaxes
            (statements->on-old-new-syntax
              (syntaxes statement ...)))))))
)
