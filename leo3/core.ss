(library (leo3 core)
  (export import export library top-level-program)
  (import
    (rename (scheme)
      (import %import)
      (export %export)
      (library %library)
      (top-level-program %top-level-program))
    (procedure)
    (syntax)
    (leo3 transformer))

  (define-syntax (import $syntax)
    (syntax-case $syntax ()
      ((import spec ...)
        #`(%import
          #,@(map
            (partial transform-import-spec #'import)
            #'(spec ...))))))

  (define-syntax (export $syntax)
    (syntax-case $syntax ()
      ((export spec ...)
        #`(%export
          #,@(map
            (partial transform-export-spec #'export)
            #'(spec ...))))))

  (define-syntax (library $syntax)
    (syntax-case $syntax ()
      ((library name export import body ...)
        #`(%library
          #,(transform-name #'name)
          #,(transform-export #'library #'export)
          #,(transform-import #'library #'import)
          body ...))))

  (define-syntax (top-level-program $syntax)
    (syntax-case $syntax ()
      ((top-level-program import body ...)
        #`(%top-level-program
          #,(transform-import #'top-level-program #'import)
          body ...))))
)
