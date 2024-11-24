(library (micalog inits-updates)
  (export
    statement->inits-updates-syntax
    statements->inits-updates-syntax)
  (import
    (micascheme)
    (prefix (micalog keywords) %))

  (define (statement->inits-updates-syntax $statement)
    (inits-updates->syntax (statement->inits-updates $statement)))

  (define (statements->inits-updates-syntax $statements)
    (inits-updates->syntax (statements->inits-updates $statements)))

  (define (inits-updates->syntax (pair $inits $updates))
    #`(begin #,@$inits #,@$updates))

  (define (statement->inits-updates $statement)
    (syntax-case $statement (%input %register %cond %on)
      ((%input body ...)
        (pair (list $statement) (list)))
      ((%register body ...)
        (pair (list $statement) (list)))
      ((%cond clause ...)
        (lets
          ($clause-pairs
            (map clause->inits-clause (syntaxes clause ...)))
          ($inits (apply append (map car $clause-pairs)))
          ($clauses (map cdr $clause-pairs))
          (pair
            $inits
            (list #`(%cond #,@$clauses)))))
      ((%on event process ...)
        (lets
          ($process-pairs
            (map clause->inits-clause (syntaxes process ...)))
          ($inits (apply append (map car $process-pairs)))
          ($processes (map cdr $process-pairs))
          (pair $inits (list #`(%on event #,@$processes)))))
      (other
        (pair (list) (list #'other)))))

  (define (statements->inits-updates $statements)
    (lets
      ($pairs (map statement->inits-updates $statements))
      (pair
        (apply append (map car $pairs))
        (apply append (map cdr $pairs)))))

  (define (clause->inits-clause $clause)
    (syntax-case $clause ()
      ((cond statement ...)
        (lets
          ((pair $inits $updates)
            (statements->inits-updates (syntaxes statement ...)))
          (pair $inits #`(cond #,@$updates))))))
)
