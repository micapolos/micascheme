(library (leo3 core)
  (export import export library)
  (import
    (rename (scheme)
      (import %import)
      (export %export)
      (library %library))
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
)
