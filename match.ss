(library (match)
  (export
    define-match-prim?
    match-prim?
    matcher
    if-matches
    match?
    match-case?
    define-predicate-match-prim?)
  (import (scheme) (syntax) (syntaxes))

  ; TODO: Define matchers in (data), for predicate and constructor
  ; TODO: Define core matchers and export in (micascheme)

  (define-lookup-syntax (match-prim? $syntax $lookup)
    (syntax-case $syntax ()
      ((_ val (id arg ...) body)
        (for-all identifier? (syntaxes val id arg ...))
        (
          (or
            ($lookup #'id #'match-prim?)
            (syntax-error #'id "match-prim? not defined for"))
          $syntax))))

  (define-rule-syntax (define-match-prim? name value)
    (define-property name match-prim? value))

  (define-lookup-syntax (matcher $syntax $lookup)
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
               ($args (syntaxes arg ...))
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
                  #`(matcher
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
                          #`(matcher #,$tmp? #,$arg #,$body)))
                      #'body
                      $tmps?
                      $args))))))
          (other (syntax-error #'other) "invalid matcher spec")))))

  (define-rule-syntax (if-matches expr spec match-body else-body)
    (or
      (matcher expr spec match-body)
      else-body))

  (define-rules-syntax
    ((match-val? val)
      #f)
    ((match-val? val (spec match-body) rules ...)
      (if-matches val spec match-body
        (match-val? val rules ...))))

  (define-rule-syntax (match-case? expr rule ...)
    (let ((val expr))
      (match-val? val rule ...)))

  (define-rule-syntax (match? expr spec body)
    (let ((val expr))
      (match-val? val (spec body))))

  (define-rule-syntax (define-predicate-match-prim? test?)
    (define-property test? match-prim?
      (lambda ($syntax)
        (syntax-case $syntax ()
          ((_ expr (_ x) body)
            #`(let ((val expr))
              (and (test? val)
                (let ((x val)) body))))))))
)
