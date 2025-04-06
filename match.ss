(library (match)
  (export
    matcher
    if-matches
    match
    define-predicate-matcher)
  (import (scheme) (syntax) (syntaxes))

  ; TODO: Define matchers in (data), for predicate and constructor
  ; TODO: Define core matchers and export in (micascheme)

  (define-lookup-syntax (matcher $syntax $lookup)
    (syntax-case $syntax ()
      ((_ expr spec body)
        (syntax-case #'spec ()
          (id
            (identifier? #'id)
            #`(let ((id expr))
              (lambda () body)))
          (x
            (let (($x (datum x)))
              (or (boolean? $x) (char? $x) (number? $x) (string? $x)))
            #`(and (equal? expr x)
              (lambda () body)))
          ((id arg ...)
            (identifier? #'id)
            (let*
              (($matcher
                (or
                  ($lookup #'id #'matcher)
                  (syntax-error #'id "no matcher")))
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
                #,($matcher
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
    (let
      (($cont? (matcher expr spec match-body)))
      (if $cont? ($cont?) else-body)))

  (define-rules-syntax
    ((match-val val)
      #f)
    ((match-val val (spec match-body) rules ...)
      (if-matches val spec match-body
        (match-val val rules ...))))

  (define-rule-syntax (match expr rule ...)
    (let ((val expr))
      (match-val val rule ...)))

  (define-rule-syntax (define-predicate-matcher test?)
    (define-property test? matcher
      (lambda ($syntax)
        (syntax-case $syntax ()
          ((_ expr (_ x) body)
            #`(let ((val expr))
              (and (test? val)
                (lambda ()
                  (let ((x val)) body)))))))))
)
