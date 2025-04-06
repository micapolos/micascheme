(library (match)
  (export
    define-match-prim?
    match-prim?
    match?
    match-case?
    match
    match-case)
  (import (scheme) (syntax))

  (define-lookup-syntax (match-prim? $syntax $lookup)
    (syntax-case $syntax ()
      ((_ val (id arg ...) body)
        (for-all identifier? (syntax->list #'(val id arg ...)))
        (
          (or
            ($lookup #'id #'match-prim?)
            (syntax-error #'id "match-prim? not defined for"))
          $syntax))))

  (define-rule-syntax (define-match-prim? name value)
    (define-property name match-prim? value))

  (define-lookup-syntax (match-prim-transitive? $syntax $lookup)
    (syntax-case $syntax ()
      ((_ expr spec body)
        (syntax-case #'spec ()
          (id
            (identifier? #'id)
            #`(let ((id expr)) body))
          (x
            (let (($x (datum x)))
              (or (boolean? $x) (char? $x) (number? $x) (string? $x)))
            #`(and (equal? expr x) body))
          ((id arg ...)
            (identifier? #'id)
            (let*
              (($match-prim?
                (or
                  ($lookup #'id #'match-prim?)
                  (syntax-error #'id "match-prim? not defined for")))
               ($args (syntax->list #'(arg ...)))
               ($tmps?
                (map
                  (lambda ($arg)
                    (and
                      (not (identifier? $arg))
                      (car (generate-temporaries $arg))))
                  $args)))
              #`(let
                (#,@(filter
                  (lambda ($id) $id)
                  (map
                    (lambda ($tmp? $arg)
                      (and $tmp? #`(#,$tmp? #'$arg)))
                    $tmps?
                    $args)))
                #,($match-prim?
                  #`(match-prim-transitive?
                    expr
                    (id
                      #,@(map
                        (lambda ($tmp? $arg)
                          (or $tmp? $arg))
                        $tmps? $args))
                    #,(fold-left
                      (lambda ($body $tmp? $arg)
                        (if (not $tmp?)
                          $body
                          #`(match-prim-transitive? #,$tmp? #,$arg #,$body)))
                      #'body
                      $tmps?
                      $args))))))
          (other (syntax-error #'other) "invalid matcher spec")))))

  (define-rule-syntax (match? expr spec body)
    (let ((val expr))
      (match-prim-transitive? val spec body)))

  (define-rule-syntax (match expr spec body)
    (or
      (match? expr spec body)
      (syntax-error #'expr "no match for")))

  (define-rule-syntax (match-case? expr (spec body) ...)
    (let ((val expr))
      (or (match? val spec body) ...)))

  (define-rule-syntax (match-case expr (spec body) ...)
    (or
      (match-case? expr (spec body) ...)
      (syntax-error #'expr "no match for")))
)
