(library (fluent)
  (export fluent)
  (import
    (scheme)
    (syntax))

  (define-rule-syntax (fluent $expr ($op $arg ...) ...)
    (let*
      (
        ($var $expr)
        ($var ($op $var $arg ...)) ...)
      $var)))
