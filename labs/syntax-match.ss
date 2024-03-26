(library (labs syntax-match)
  (export
    pattern-literal?
    pattern-matcher

    parse-pattern
    parse-pattern-match)
  (import (micascheme))

  (define-aux-keyword pattern-matcher)
  (define-aux-keyword pattern-literal?)

  (define $... (datum->syntax #'$... '...))

  (define (parse-pattern $lookup $pattern)
    (syntax-case $pattern ()
      ($id
        (and
          (identifier? #'$id)
          ($lookup #'$id #'pattern-literal?))
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
          ($lookup #'$id #'pattern-matcher))
        (app
          ($lookup #'$id #'pattern-matcher)
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
        (syntax-error #'$other "invalid pattern"))))

  (define (parse-pattern-match $lookup $syntax $pattern $body)
    (lets
      ((values $params $args-proc)
        (parse-pattern $lookup $pattern))
      #`(opt-lets
        ($args (#,$args-proc #,$syntax))
        (syntax-case #`(#,@$args) ()
          ((#,@$params) #,$body)))))
)
