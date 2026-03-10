(library (leo transform)
  (export
    transform-name
    transform-export
    transform-import
    transform-spec
    transform-library)
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
    (syntax-case $syntax (library)
      ((_ name exports imports body ...)
        #`(library
          #,(transform-name #'name)
          #,(transform-export #'exports)
          #,(transform-import #'imports)
          body ...))))

)
