(library (leo library)
  (export
    library import export top-level-program
    except only rename alias prefix add-prefix drop-prefix
    from)
  (import
    (rename (micascheme)
      (import %import)
      (export %export)
      (except %except)
      (only %only)
      (rename %rename)
      (alias %alias)
      (prefix %prefix)
      (add-prefix %add-prefix)
      (drop-prefix %drop-prefix)
      (library %library)
      (top-level-program %top-level-program)
      (from %from))
    (keyword)
    (condition))

  (meta define (transform-identifier $syntax)
    (syntax-case $syntax ()
      (id (keyword? id) #'id)
      (_ (syntax-error $syntax "invalid identifier"))))

  (meta define (transform-mapping $syntax)
    (syntax-case $syntax ()
      ((from to)
        #`(
          #,(transform-identifier #'from)
          #,(transform-identifier #'to)))
      (_
        (syntax-error $syntax "invalid mapping spec"))))

  (meta define (transform-name $syntax)
    (syntax-case $syntax ()
      ((id x)
        #`(
          #,(transform-identifier #'id) . #,(transform-name #'x)))
      (id
        #`(#,(transform-identifier #'id)))))

  (meta define (transform-import-spec $syntax)
    (syntax-case $syntax (except only rename alias prefix add-prefix drop-prefix from)
      ((except id ... (from spec))
        #`(%except
          #,(transform-import-spec #'spec)
          #,@(map transform-identifier #'(id ...))))
      ((only id ... (from spec))
        #`(%only
          #,(transform-import-spec #'spec)
          #,@(map transform-identifier #'(id ...))))
      ((rename mapping ... (from import-spec))
        #`(%rename
          #,(transform-import-spec #'import-spec)
          #,@(map transform-mapping #'(mapping ...))))
      ((alias mapping ... (from import-spec))
        #`(%alias
          #,(transform-import-spec #'import-spec)
          #,@(map transform-mapping #'(mapping ...))))
      ((prefix (id (from import-spec)))
        #`(%prefix
          #,(transform-import-spec #'import-spec)
          #,(transform-identifier #'id)))
      ((add-prefix (id (from import-spec)))
        #`(%add-prefix
          #,(transform-import-spec #'import-spec)
          #,(transform-identifier #'id)))
      ((drop-prefix (id (from import-spec)))
        #`(%drop-prefix
          #,(transform-import-spec #'import-spec)
          #,(transform-identifier #'id)))
      (name
        (transform-name #'name))))

  (meta define (transform-export-spec $syntax)
    (syntax-case $syntax (rename import)
      ((rename mapping ...)
        #`(%rename
          #,@(map transform-mapping #'(mapping ...))))
      ((import spec ...)
        #`(%import
          #,@(map transform-import-spec #'(spec ...))))
      (id
        (transform-identifier #'id))))

  (meta define (transform-export $syntax)
    (syntax-case $syntax (export)
      ((export spec ...)
        #`(%export
          #,@(map transform-export-spec #'(spec ...))))
      (_
        (syntax-error $syntax "expected export"))))

  (meta define (transform-import $syntax)
    (syntax-case $syntax (import)
      ((import spec ...)
        #`(%import
          #,@(map transform-import-spec #'(spec ...))))
      (_
        (raise
          (condition
            (make-syntax-violation $syntax #f)
            (make-cause-condition `(expected import)))))))

  (meta define (transform-rename-spec $syntax)
    (syntax-case $syntax ()
      ((from to)
        #`(
          #,(transform-identifier #'from)
          #,(transform-identifier #'to)))
      (_
        (syntax-error $syntax "invalid rename spec"))))

  (meta define (transform-library $syntax)
    (syntax-case $syntax (library)
      ((library name exports imports body ...)
        #`(%library
          #,(transform-name #'name)
          #,(transform-export #'exports)
          #,(transform-import #'imports)
          body ...))
      (_
        (syntax-error $syntax "expected library"))))

  (meta define (transform-top-level-program $syntax)
    (syntax-case $syntax (top-level-program)
      ((top-level-program imports body ...)
        #`(%top-level-program
          #,(transform-import #'imports)
          body ...))
      (_
        (syntax-error $syntax "invalid top level program"))))

  (define-keywords except only rename alias prefix add-prefix drop-prefix from)

  (define-syntax library transform-library)
  (define-syntax import transform-import)
  (define-syntax export transform-export)
  (define-syntax top-level-program transform-top-level-program)
)
