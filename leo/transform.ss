(library (leo transform)
  (export
    transform-name
    transform-export
    transform-import
    transform-export-spec
    transform-import-spec
    transform-library
    transform-define
    transform-lambda
    transform-with
    transform-leo)
  (import
    (micascheme)
    (keyword))

  (define (transform-identifier $syntax)
    (syntax-case $syntax ()
      (id (keyword? id) #'id)
      (_ (syntax-error $syntax "invalid identifier"))))

  (define (transform-name $syntax)
    (syntax-case $syntax ()
      ((id x)
        #`(
          #,(transform-identifier #'id) . #,(transform-name #'x)))
      (id
        #`(#,(transform-identifier #'id)))))

  (define (transform-export $syntax)
    (syntax-case $syntax ()
      ((_ spec ...)
        #`(export
          #,@(map transform-export-spec #'(spec ...))))
      (_
        (syntax-error $syntax "invalid export"))))

  (define (transform-import $syntax)
    (syntax-case $syntax ()
      ((_ spec ...)
        #`(import
          #,@(map transform-import-spec #'(spec ...))))
      (_
        (syntax-error $syntax "invalid import"))))

  (define (transform-rename-spec $syntax)
    (syntax-case $syntax ()
      ((from to)
        #`(
          #,(transform-identifier #'from)
          #,(transform-identifier #'to)))
      (_
        (syntax-error $syntax "invalid rename spec"))))

  (define (transform-export-spec $syntax)
    (syntax-case $syntax (rename import)
      ((rename spec ...)
        #`(rename
          #,@(map transform-rename-spec #'(spec ...))))
      ((import spec ...)
        #`(import
          #,@(map transform-import-spec #'(spec ...))))
      (id
        (transform-identifier #'id))))

  (define (transform-import-spec $syntax)
    (syntax-case $syntax (except only rename prefix)
      ((except id ... spec)
        #`(except
          #,(transform-import-spec #'spec)
          #,@(map transform-identifier #'(id ...))))
      ((only id ... spec)
        #`(only
          #,(transform-import-spec #'spec)
          #,@(map transform-identifier #'(id ...))))
      ((rename rename-spec ... import-spec)
        #`(rename
          #,(transform-import-spec #'import-spec)
          #,@(map transform-rename-spec #'(rename-spec ...))))
      ((prefix (id import-spec))
        #`(prefix
          #,(transform-import-spec #'import-spec)
          #,(transform-identifier #'id)))
      (name
        (transform-name #'name))))

  (define (transform-library $syntax)
    (syntax-case $syntax ()
      ((_ name exports imports body ...)
        #`(library
          #,(transform-name #'name)
          #,(transform-export #'exports)
          #,(transform-import #'imports)
          body ...))
      (_
        (syntax-error $syntax "invalid library"))))

  (define (transform-top-level-program $syntax)
    (syntax-case $syntax ()
      ((_ imports body ...)
        #`(top-level-program
          #,(transform-import #'imports)
          body ...))
      (_
        (syntax-error $syntax "invalid top level program"))))

  (define (transform-define $syntax)
    (syntax-case $syntax ()
      ((_ (id x))
        #`(define #,(transform-identifier #'id) x))
      ((_ (id x ...) body)
        #`(define (#,(transform-identifier #'id) x ...) body))
      (_
        (syntax-error $syntax "invalid define"))))

  (define (transform-lambda $syntax)
    (syntax-case $syntax ()
      ((_ param ... body)
        #`(lambda (#,@(map transform-identifier #'(param ...))) body))
      (_
        (syntax-error $syntax "invalid lambda"))))

  (define (transform-with $syntax)
    (syntax-case $syntax ()
      ((_ x) #`(#,(transform-identifier #'x)))))

  (define (transform-leo $syntax)
    (syntax-case $syntax (library import define lambda with top-level-program)
      (("noexpand" . x) $syntax)
      ((library . x) (transform-library $syntax))
      ((import . x) (transform-import $syntax))
      ((define . x) (transform-define $syntax))
      ((lambda . x) (transform-lambda $syntax))
      ((with . x) (transform-with $syntax))
      ((top-level-program . x) (transform-top-level-program $syntax))
      (_ (syntax-error $syntax "invalid leo"))))
)
