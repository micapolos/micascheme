(library (labs pattern-match)
  (export
    syntax-literal?
    syntax-matcher

    parse-pattern
    parse-pattern-clause
    parse-pattern-clauses
    parse-pattern-clauses-2

    syntax-matcher-2
    parse-pattern-2)
  (import (micascheme))

  (define-aux-keyword syntax-matcher)
  (define-aux-keyword syntax-matcher-2)
  (define-aux-keyword syntax-literal?)

  (define $... (datum->syntax #'$... '...))

  (define (parse-pattern $lookup $pattern)
    (syntax-case $pattern ()
      ($id
        (and
          (identifier? #'$id)
          ($lookup #'$id #'syntax-literal?))
        (values
          (list)
          #`(lambda ($syntax)
            (and
              (identifier? $syntax)
              (free-identifier=? $syntax #'$id)
              (list)))))
      (($id . $tail)
        (and
          (identifier? #'$id)
          ($lookup #'$id #'syntax-matcher))
        (app
          ($lookup #'$id #'syntax-matcher)
          $pattern))
      ($other
        (parse-default-pattern $lookup #'$other))))

  (define (parse-default-pattern $lookup $pattern)
    (syntax-case $pattern ()
      (()
        (values
          (list)
          #`(lambda ($syntax)
            (syntax-case-opt $syntax ()
              (() (list))))))
      (($head . $tail)
        (lets
          ((values $head-params $head-proc) (parse-pattern $lookup #'$head))
          ((values $tail-params $tail-proc) (parse-default-pattern $lookup #'$tail))
          (values
            (append $head-params $tail-params)
            #`(lambda ($syntax)
              (syntax-case-opt $syntax ()
                (($syntax-head . $syntax-tail)
                  (opt-lets
                    ($head-args (#,$head-proc #'$syntax-head))
                    ($tail-args (#,$tail-proc #'$syntax-tail))
                    (append $head-args $tail-args))))))))
      ($id
        (identifier? #'$id)
        (values
          (list #'$id)
          #'(lambda ($syntax) (list $syntax))))
      ($other
        (with-implicit (parse-default-pattern $tmp)
          (values
            (list)
            #`(lambda ($syntax)
              (and
                (equal? (syntax->datum $syntax) '$other)
                (list))))))))

  (define (parse-pattern-clause $lookup $syntax $clause)
    (syntax-case $clause ()
      (($pattern $body)
        (lets
          ((values $params $args-proc)
            (parse-pattern $lookup #'$pattern))
          #`(opt-lets
              ($args (#,$args-proc #,$syntax))
              (syntax-case #`(#,@$args) ()
                ((#,@$params) $body)))))))

  (define (parse-pattern-clauses $lookup $syntax $clauses)
    #`(or
      #,@(map
        (partial parse-pattern-clause $lookup $syntax)
        $clauses)))

  ; =================================================================

  (define (parse-pattern-2 $lookup $pattern)
    (syntax-case $pattern ()
      ($id
        (and
          (identifier? #'$id)
          ($lookup #'$id #'syntax-literal?))
        (lambda ($body)
          #`(lambda ($syntax)
            (and
              (identifier? $syntax)
              (free-identifier=? $syntax #'$id)
              #,$body))))
      (($id . $tail)
        (and
          (identifier? #'$id)
          ($lookup #'$id #'syntax-matcher-2))
        (app
          ($lookup #'$id #'syntax-matcher-2)
          $pattern))
      ($other
        (parse-default-pattern-2 $lookup #'$other))))

  (define (parse-default-pattern-2 $lookup $pattern)
    (syntax-case $pattern ()
      (()
        (lambda ($body)
          #`(lambda ($syntax)
            (syntax-case-opt $syntax ()
              (() #,$body)))))
      (($head . $tail)
        (lets
          ($head-parse (parse-pattern-2 $lookup #'$head))
          ($tail-parse (parse-default-pattern-2 $lookup #'$tail))
          (lambda ($body)
            (lets
              ($body ($head-parse $body))
              ($body ($tail-parse $body))
              #`(lambda ($syntax)
                (syntax-case-opt $syntax ()
                  (($syntax-head . $syntax-tail)
                    (opt-lets
                      ($result (app #,$body #'$syntax-tail))
                      (app $result #'$syntax-head)))))))))
      ($id
        (identifier? #'$id)
        (lambda ($body)
          #`(lambda ($syntax)
            (with-syntax (($id $syntax))
              #,$body))))
      ($other
        (lambda ($body)
          #`(lambda ($syntax)
            (and
              (equal? (syntax->datum $syntax) '$other)
              #,$body))))))


  (define (parse-pattern-clause-2 $lookup $syntax $clause)
    (syntax-case $clause ()
      (($pattern $body)
        #`(app
          #,(app (parse-pattern-2 $lookup #'$pattern) #'$body)
          #,$syntax))))

  (define (parse-pattern-clauses-2 $lookup $syntax $clauses)
    #`(lets
      ($syntax-var #,$syntax)
      (or
        #,@(map
          (partial parse-pattern-clause-2 $lookup #'$syntax-var)
          $clauses))))
)
