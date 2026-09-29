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
    (leo3 rewriter))

  (define-syntax (import $syntax)
    (syntax-case $syntax ()
      ((import spec ...)
        #`(%import
          #,@(map
            (dot (partial datum->syntax #'import) rewrite-import-spec syntax->datum)
            #'(spec ...))))))

  (define-syntax (export $syntax)
    (syntax-case $syntax ()
      ((export spec ...)
        #`(%export
          #,@(map
            (dot (partial datum->syntax #'export) rewrite-export-spec syntax->datum)
            #'(spec ...))))))

  (define-syntax (library $syntax)
    (syntax-case $syntax ()
      ((library name export import body ...)
        #`(%library
          #,(datum->syntax #'library (rewrite-name (syntax->datum #'name)))
          #,(datum->syntax #'library (rewrite-export (syntax->datum #'export)))
          #,(datum->syntax #'library (rewrite-import (syntax->datum #'import)))
          body ...))))

  (define-syntax (top-level-program $syntax)
    (syntax-case $syntax ()
      ((top-level-program import body ...)
        #`(%top-level-program
          #,(datum->syntax #'top-level-program (rewrite-import (syntax->datum #'import)))
          body ...))))
)
