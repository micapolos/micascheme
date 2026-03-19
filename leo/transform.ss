(library (leo transform)
  (export
    from with
    transform-name
    transform-export
    transform-import
    transform-export-spec
    transform-import-spec
    transform-library
    transform-leo)
  (import
    (except (micascheme) from with)
    (keyword))

  (define-keywords from with)

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

  (define (transform-mapping $syntax)
    (syntax-case $syntax ()
      ((from to)
        #`(
          #,(transform-identifier #'from)
          #,(transform-identifier #'to)))
      (_
        (syntax-error $syntax "invalid mapping spec"))))

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
    (syntax-case $syntax (except only rename prefix from)
      ((except id ... (from spec))
        #`(except
          #,(transform-import-spec #'spec)
          #,@(map transform-identifier #'(id ...))))
      ((only id ... (from spec))
        #`(only
          #,(transform-import-spec #'spec)
          #,@(map transform-identifier #'(id ...))))
      ((rename mapping ... (from import-spec))
        #`(rename
          #,(transform-import-spec #'import-spec)
          #,@(map transform-mapping #'(mapping ...))))
      ((alias mapping ... (from import-spec))
        #`(alias
          #,(transform-import-spec #'import-spec)
          #,@(map transform-mapping #'(mapping ...))))
      ((prefix (id (from import-spec)))
        #`(prefix
          #,(transform-import-spec #'import-spec)
          #,(transform-identifier #'id)))
      ((add-prefix (id (from import-spec)))
        #`(add-prefix
          #,(transform-import-spec #'import-spec)
          #,(transform-identifier #'id)))
      ((drop-prefix (id (from import-spec)))
        #`(drop-prefix
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

  (define (transform-leo $syntax)
    (syntax-case $syntax (library import define lambda with top-level-program)
      (("noexpand" . x) $syntax)
      ((library . x) (transform-library $syntax))
      ((import . x) (transform-import $syntax))
      ((top-level-program . x) (transform-top-level-program $syntax))
      (_ $syntax)))
)
