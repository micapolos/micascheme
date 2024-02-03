(library (fluent)
  (export fluent)
  (import
    (scheme)
    (syntax))

  (define-syntax-rule (fluent $expr ($op $arg ...) ...)
    (let*
      (
        ($var $expr)
        ($var ($op $var $arg ...)) ...)
      $var)))
