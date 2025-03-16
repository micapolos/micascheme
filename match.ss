(library (match)
  (export
    matcher
    if-matches match)
  (import (scheme) (syntax) (syntaxes) (throw) (identifier))

  (define-lookup-syntax (matcher $syntax $lookup)
    (syntax-case $syntax ()
      ((_ expr id body)
        (identifier? #'id)
        #`(lambda ()
          (let ((id expr)) body)))
      ((_ expr (id . rest) body)
        (identifier? #'id)
        (let*
          (($matcher
            (or
              ($lookup #'id #'matcher)
              (syntax-error #'id "no matcher"))))
          ($matcher $syntax)))))

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
)
