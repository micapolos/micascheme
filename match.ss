(library (match)
  (export
    matcher
    if-matches
    match
    define-predicate-matcher)
  (import (scheme) (syntax) (syntaxes) (throw) (identifier))

  ; TODO: Make matcher recursive, so we can match not only on 1-level deep matchers.
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
          ((id . rest)
            (identifier? #'id)
            (let*
              (($matcher
                (or
                  ($lookup #'id #'matcher)
                  (syntax-error #'id "no matcher"))))
              ($matcher $syntax)))
          (other (syntax-error #'other) "invalid matcher spec")))))

  (define-rule-syntax (if-matches expr spec match-body else-body)
    (let
      (($cont? (matcher expr spec match-body)))
      (if $cont? ($cont?) else-body)))

  (define-rules-syntax
    ((match-val val)
      (throw mismatch))
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
