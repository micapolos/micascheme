(library (leo transform)
  (export
    transform-name
    transform-export
    transform-import
    transform-spec
    transform-library
    transform-define
    transform-lambda
    transform-with
    transform-lang)
  (import (micascheme))

  (define (transform-name $syntax)
    (syntax-case $syntax ()
      ((id x) #`(id . #,(transform-name #'x)))
      (id #'(id))))

  (define (transform-export $syntax)
    (syntax-case $syntax ()
      ((_ id ...) #`(export id ...))))

  (define (transform-import $syntax)
    (syntax-case $syntax ()
      ((_ spec ...)
        #`(import
          #,@(map transform-spec #'(spec ...))))))

  (define (transform-spec $syntax)
    (syntax-case $syntax (except only rename prefix)
      ((except spec id ...)
        #`(except
          #,(transform-spec #'spec)
          id ...))
      ((only spec id ...)
        #`(only
          #,(transform-spec #'spec)
          id ...))
      ((rename spec (from to) ...)
        #`(rename
          #,(transform-spec #'spec)
          (from to) ...))
      ((prefix spec id)
        #`(prefix
          #,(transform-spec #'spec)
          id))
      (name (transform-name #'name))))

  (define (transform-library $syntax)
    (syntax-case $syntax ()
      ((_ name exports imports body ...)
        #`(library
          #,(transform-name #'name)
          #,(transform-export #'exports)
          #,(transform-import #'imports)
          body ...))))

  (define (transform-top-level-program $syntax)
    (syntax-case $syntax ()
      ((_ imports body ...)
        #`(top-level-program
          #,(transform-import #'imports)
          body ...))))

  (define (transform-define $syntax)
    (syntax-case $syntax ()
      ((_ (id x))
        #`(define id x))
      ((_ (id x ...) body)
        #`(define (id x ...) body))))

  (define (transform-lambda $syntax)
    (syntax-case $syntax ()
      ((_ param ... body)
        #`(lambda (param ...) body))))

  (define (transform-with $syntax)
    (syntax-case $syntax ()
      ((_ x)
        #`(x))))

  (define (transform-lang $syntax)
    (syntax-case $syntax (library import define lambda with top-level-program)
      (("noexpand" . x) $syntax)
      ((library . x) (transform-library $syntax))
      ((import . x) (transform-import $syntax))
      ((define . x) (transform-define $syntax))
      ((lambda . x) (transform-lambda $syntax))
      ((with . x) (transform-with $syntax))
      ((top-level-program . x) (transform-top-level-program $syntax))))
)
