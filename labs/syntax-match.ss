(library (labs syntax-match)
  (export
    define-literal?
    define-syntax-matcher
    syntax-match
    match)
  (import (micascheme))

  (define-aux-keyword syntax-literal?)
  (define-aux-keyword syntax-matcher)

  (define null-match
    (lambda ($key) #f))

  (define (match-put $match $id $value)
    (lambda ($key)
      (or
        (and (free-identifier=? $key $id) $value)
        ($match $key))))

  (define (match-append $match-a $match-b)
    (lambda ($key)
      (or
        ($match-b $key)
        ($match-a $key))))

  (define-syntax match
    (lambda ($syntax)
      (syntax-case $syntax ()
        ((_ $entry ...)
          (fold-left
            (lambda ($match $entry)
              (syntax-case $entry ()
                (($id $value)
                  #`(lambda ($key)
                    (or
                      (and (free-identifier=? $key #'$id) #'$value)
                      (#,$match $key))))))
            #`(lambda ($key) #f)
            (syntax->list #'($entry ...)))))))

  (define-syntax-rule (define-literal? $name)
    (define-property $name syntax-literal? #t))

  (define-syntax-rule (define-syntax-matcher $name $matcher)
    (define-property $name syntax-matcher $matcher))

  (define (syntax-match-pattern-top-level $lookup $syntax $pattern)
    (syntax-case $pattern ()
      (($id $param ...)
        (and (identifier? #'$id))
        (lets
          ($matcher ($lookup #'$id #'syntax-matcher))
          (if $matcher
            ($matcher $lookup $syntax $pattern)
            (syntax-match-pattern $lookup $syntax $pattern))))
      ($other (syntax-match-pattern $lookup $syntax $pattern))))

  (define (syntax-match-pattern $lookup $syntax $pattern)
    (syntax-case $pattern ()
      ($id
        (identifier? #'$id)
        (if ($lookup #'$id #'syntax-literal?)
          (and
            (identifier? $syntax)
            (free-identifier=? $pattern $syntax)
            null-match)
          (lambda ($key)
            (and (bound-identifier=? $key #'$id) $syntax))))
      (()
        (syntax-case $syntax ()
          (() null-match)
          (_ #f)))
      (($pattern-head . $pattern-tail)
        (syntax-case $syntax ()
          (($head . $tail)
            (opt-lets
              ($head-match (syntax-match-pattern-top-level $lookup #'$head #'$pattern-head))
              ($tail-match (syntax-match-pattern $lookup #'$tail #'$pattern-tail))
              (match-append $head-match $tail-match)))
          (_ #f)))))

  (define (syntax-match-1 $lookup $syntax $pattern $body)
    (opt-lets
      ($match (syntax-match-pattern-top-level $lookup $syntax $pattern))
      (syntax-match-apply $match $body)))

  (define (syntax-match $lookup $syntax $entries)
    (fold-left
      (lambda ($acc $entry)
        (or $acc
          (syntax-case $entry ()
            (($pattern $body)
              (syntax-match-1 $lookup $syntax #'$pattern #'$body)))))
      #f
      $entries))

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
        (or ($match #'$id) #'$id))
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
