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
    (syntax-case $statement (%input %output %wire %register %set %cond %else %on)
      ((%input body ...)
        (pair (list $statement) (list)))
      ((%output body ...)
        (pair (list) (list $statement)))
      ((%wire body ...)
        (pair (list) (list $statement)))
      ((%register body ...)
        (pair (list $statement) (list)))
      ((%set body ...)
        (pair (list) (list $statement)))
      ((%cond clause ... (%else statement ...))
        (lets
          ($clause-pairs
            (map clause->inits-clause (syntaxes clause ...)))
          ($inits (apply append (map car $clause-pairs)))
          ($clauses (map cdr $clause-pairs))
          ((pair $else-inits $else-updates)
            (statements->inits-updates (syntaxes statement ...)))
          (pair
            (append $inits $else-inits)
            (list #`(%cond #,@$clauses (%else #,@$else-updates))))))
      ((%cond clause-1 clause ...)
        (lets
          ($clause-pairs
            (map clause->inits-clause (syntaxes clause-1 clause ...)))
          ($inits (apply append (map car $clause-pairs)))
          ($clauses (map cdr $clause-pairs))
          (pair
            $inits
            (list #`(%cond #,@$clauses)))))
      ((%on event process)
        (lets
          ((pair $inits $process)
            (process->inits-process #'process))
          (pair $inits (list #`(%on event #,$process)))))
      ((%on event process other-process)
        (lets
          ((pair $inits $process)
            (process->inits-process #'process))
          ((pair $other-inits $other-process)
            (process->inits-process #'other-process))
          (pair
            (append $inits $other-inits)
            (list #`(%on event #,$process #,$other-process)))))))

  (define (statements->inits-updates $statements)
    (lets
      ($pairs (map statement->inits-updates $statements))
      (pair
        (apply append (map car $pairs))
        (apply append (map cdr $pairs)))))

  (define (process->inits-process $process)
    (syntax-case $process ()
      ((edge statement ...)
        (lets
          ((pair $inits $updates)
            (statements->inits-updates (syntaxes statement ...)))
          (pair $inits #`(edge #,@$updates))))))

  (define (clause->inits-clause $clause)
    (syntax-case $clause ()
      ((cond statement ...)
        (lets
          ((pair $inits $updates)
            (statements->inits-updates (syntaxes statement ...)))
          (pair $inits #`(cond #,@$updates))))))
)
