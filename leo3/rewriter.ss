(library (leo3 rewriter)
  (export
    rewrite-identifier
    rewrite-identifier-pair
    rewrite-name
    rewrite-import
    rewrite-export
    rewrite-import-spec
    rewrite-export-spec
    rewrite-library)
  (import
    (scheme)
    (symbol))

  (define (rewrite-identifier $id)
    (syntax-case $id ()
      (id
        (symbol? (datum id))
        #'id)))

  (define (rewrite-name $name)
    (syntax-case $name (name)
      ((name id ...)
        (map rewrite-identifier #'(id ...)))))

  (define (rewrite-import $import)
    (syntax-case $import (import)
      ((import spec ...)
        `(import ,@(map rewrite-import-spec #'(spec ...))))))

  (define (rewrite-export $export)
    (syntax-case $export (export)
      ((export spec ...)
        `(export ,@(map rewrite-export-spec #'(spec ...))))))

  (define (rewrite-import-spec $spec)
    (syntax-case $spec (from only except prefix rename)
      ((from xs ...)
        (map rewrite-identifier #'(xs ...)))
      ((only ids ... spec)
        `(only
          ,(rewrite-import-spec #'spec)
          ,@(map rewrite-identifier #'(ids ...))))
      ((except ids ... spec)
        `(except
          ,(rewrite-import-spec #'spec)
          ,@(map rewrite-identifier #'(ids ...))))
      ((prefix id spec)
        `(prefix
          ,(rewrite-import-spec #'spec)
          ,(symbol-append (rewrite-identifier #'id) '-)))
      ((rename xs ... spec)
        `(rename
          ,(rewrite-import-spec #'spec)
          ,@(map rewrite-identifier-pair #'(xs ...))))))

  (define (rewrite-identifier-pair $spec)
    (syntax-case $spec ()
      ((a b)
        `(
          ,(rewrite-identifier #'a)
          ,(rewrite-identifier #'b)))))

  (define (rewrite-export-spec $spec)
    (syntax-case $spec ()
      (id (rewrite-identifier #'id))))

  (define (rewrite-library $library)
    (syntax-case $library ()
      ((library name export import body ...)
        #`(import
          #,(rewrite-name #'name)
          #,(rewrite-export #'export)
          #,(rewrite-import #'import)
          body ...))))
)
