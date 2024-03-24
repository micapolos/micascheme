(library (labs syntax-match)
  (export
    null-match
    id-match
    match-put
    match-append
    match-ref
    match
    combined-match

    syntax-literal?
    syntax-matcher

    define-literal?
    define-syntax-matcher
    syntax-match
    syntax-match-apply
    syntax-rule-id)
  (import (micascheme))

  (define-aux-keyword syntax-literal?)
  (define-aux-keyword syntax-matcher)

  (define null-match (list))

  (define (id-match $id $value)
    (match-put null-match $id $value))

  (define (match-put $match $id $value)
    (cons (cons $id $value) $match))

  (define (match-put-pair $match $pair)
    (match-put $match (car $pair) (cdr $pair)))

  (define (match-append $match-a $match-b)
    (append $match-b $match-a))

  (define (match-ref $match $id)
    (opt-lift cdr (assid $id $match)))

  (define (syntax-rule-id $rule)
    (syntax-case $rule ()
      (($id . _) (identifier? #'$id) #'$id)
      ((($id . _) . _) (identifier? #'$id) #'$id)))

  (define-syntax match
    (lambda ($syntax)
      (syntax-case $syntax ()
        ((_ $entry ...)
          #`(fold-left match-put-pair null-match
            (list
              #,@(map
                (lambda ($entry)
                  (syntax-case $entry ()
                    (($id $value)
                      #`(cons #'$id $value))))
                (syntax->list #'($entry ...)))))))))

  (define-syntax-rule (define-literal? $name)
    (define-property $name syntax-literal? #t))

  (define-syntax-rule (define-syntax-matcher $name $matcher)
    (define-property $name syntax-matcher $matcher))

  (define-syntax-rule (combined-match ($param ...) $arg ...)
    (match ($param $arg) ...))

  (define-syntax-rule (key-syntax-case $syntax ($param ...) ($key $arg ...) ...)
    (syntax-case $syntax ()
      ($key (combined-match ($param ...) ($arg ...)))
      ...
      (_ #f)))

  (define (syntax-pattern-match $lookup $syntax $pattern)
    (syntax-case $pattern ()
      ($id
        (identifier? #'$id)
        (if ($lookup #'$id #'syntax-literal?)
          (and
            (identifier? $syntax)
            (free-identifier=? $pattern $syntax)
            null-match)
          (id-match #'$id $syntax)))
      (($id $param ...)
        (and (identifier? #'$id))
        (lets
          ($matcher ($lookup #'$id #'syntax-matcher))
          (if $matcher
            ($matcher $lookup $syntax $pattern)
            (syntax-pattern-inner-match $lookup $syntax $pattern))))
      ($other
        (syntax-pattern-inner-match $lookup $syntax $pattern))))

  (define (syntax-pattern-inner-match $lookup $syntax $pattern)
    (syntax-case $pattern ()
      (()
        (syntax-case $syntax ()
          (() null-match)
          (_ #f)))
      (($pattern-head . $pattern-tail)
        (syntax-case $syntax ()
          (($head . $tail)
            (opt-lets
              ($head-match (syntax-pattern-match $lookup #'$head #'$pattern-head))
              ($tail-match (syntax-pattern-inner-match $lookup #'$tail #'$pattern-tail))
              (match-append $head-match $tail-match)))
          (_ #f)))
      ($other #'$other)))

  (define (syntax-match-1 $lookup $syntax $pattern $body)
    (opt-lets
      ($match (syntax-pattern-match $lookup $syntax $pattern))
      (syntax-match-apply $match $body)))

  (define (syntax-match $lookup $syntax $clauses)
    (or
      (fold-left
        (lambda ($acc $clause)
          (or $acc
            (syntax-case $clause ()
              (($pattern $body)
                (syntax-match-1 $lookup $syntax #'$pattern #'$body)))))
        #f
        $clauses)
      (syntax-error $syntax)))

  (define (syntax-match-apply $match $syntax)
    (depth-syntax-match-apply $match 0 $syntax))

  (define (depth-syntax-match-apply $match $depth $syntax)
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
              (depth-syntax-match-apply $match (add1 $depth) $body))
            (syntax->list #'($body ...)))))
      (($quasisyntax $body ...)
        (and
          (identifier? #'$quasisyntax)
          (free-identifier=? #'$quasisyntax #'quasisyntax))
        #`(
          $quasisyntax
          #,@(map
            (lambda ($body)
              (depth-syntax-match-apply $match (add1 $depth) $body))
            (syntax->list #'($body ...)))))
      (($unsyntax $body ...)
        (and
          (identifier? #'$unsyntax)
          (free-identifier=? #'$unsyntax #'unsyntax))
        #`(
          $unsyntax
          #,@(map
            (lambda ($body)
              (depth-syntax-match-apply $match (sub1 $depth) $body))
            (syntax->list #'($body ...)))))
      (($unsyntax-splicing $body ...)
        (and
          (identifier? #'$unsyntax-splicing)
          (free-identifier=? #'$unsyntax-splicing #'unsyntax-splicing))
        #`(
          $unsyntax-splicing
          #,@(map
            (lambda ($body)
              (depth-syntax-match-apply $match (sub1 $depth) $body))
            (syntax->list #'($body ...)))))
      (($head . $tail)
        (depth-inner-syntax-match-apply $match $depth $syntax))
      ($id
        (and (zero? $depth) (identifier? #'$id))
        (or (match-ref $match #'$id) #'$id))
      ($other #'$other)))

  (define (depth-inner-syntax-match-apply $match $depth $syntax)
    (syntax-case $syntax ()
      (() $syntax)
      (($head . $tail)
        #`(
          #,(depth-syntax-match-apply $match $depth #'$head)
          .
          #,(depth-inner-syntax-match-apply $match $depth #'$tail)))))
)
