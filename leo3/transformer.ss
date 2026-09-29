(library (leo3 transformer)
  (export
    transform-identifier
    transform-identifier-pair
    transform-name
    transform-import
    transform-export
    transform-import-spec
    transform-export-spec
    transform-library
    check-transforms
    check-transform-raises)
  (import
    (scheme)
    (syntax)
    (syntaxes)
    (keyword)
    (identifier)
    (check)
    (procedure)
    (symbol))

  ; TODO: Refactor these to transform syntax

  (define (transform-identifier $id)
    (syntax-case $id ()
      (id
        (identifier? #'id)
        #'id)
      (other
        (syntax-error #'other "invalid identifier"))))

  (define (transform-name $name)
    (syntax-case $name ()
      ((name id ...)
        (map transform-identifier #'(id ...)))
      (other
        (syntax-error #'other "invalid name"))))

  (define (transform-import $template $import)
    (syntax-case $import ()
      ((import spec ...)
        (free-keyword? import)
        #`(import
          #,@(map
            (partial transform-import-spec $template)
            #'(spec ...))))
      (other
        (syntax-error #'other "invalid import"))))

  (define (transform-export $template $export)
    (syntax-case $export ()
      ((export spec ...)
        #`(export
          #,@(map
            (partial transform-export-spec $template)
            #'(spec ...))))
      (other
        (syntax-error #'other "invalid export"))))

  (define (transform-import-spec $template $spec)
    (syntax-case $spec ()
      ((from xs ...)
        (free-keyword? from)
        (map transform-identifier #'(xs ...)))
      ((only ids ... spec)
        (free-keyword? only)
        #`(only
          #,(transform-import-spec $template #'spec)
          #,@(map transform-identifier #'(ids ...))))
      ((except ids ... spec)
        (free-keyword? except)
        #`(except
          #,(transform-import-spec $template #'spec)
          #,@(map transform-identifier #'(ids ...))))
      ((prefix id spec)
        (free-keyword? prefix)
        #`(prefix
          #,(transform-import-spec $template #'spec)
          #,(identifier-append $template (transform-identifier #'id) #'-)))
      ((rename xs ... spec)
        (free-keyword? rename)
        #`(rename
          #,(transform-import-spec $template #'spec)
          #,@(map transform-identifier-pair #'(xs ...))))
      (other
        (syntax-error #'other "invalid import spec"))))

  (define (transform-identifier-pair $spec)
    (syntax-case $spec ()
      ((a b)
        #`(
          #,(transform-identifier #'a)
          #,(transform-identifier #'b)))
      (other
        (syntax-error #'other "invalid identifier pair"))))

  (define (transform-export-spec $template $spec)
    (syntax-case $spec ()
      (id
        (transform-identifier #'id))
      (other
        (syntax-error #'other "invalid export spec"))))

  (define (transform-library $template $library)
    (syntax-case $library ()
      ((library name export import body ...)
        #`(library
          #,(transform-name #'name)
          #,(transform-export $template #'export)
          #,(transform-import $template #'import)
          body ...))
      (other
        (syntax-error #'other "invalid library"))))

  (define-rules-syntax
    ((check-transforms (fn in) out)
      (check (equal? (syntax->datum (fn #'in)) 'out)))
    ((check-transforms (fn tpl in) out)
      (check (equal? (syntax->datum (fn #'tpl #'in)) 'out))))

  (define-rules-syntax
    ((check-transform-raises (fn in))
      (check (raises (fn #'in))))
    ((check-transform-raises (fn tpl in))
      (check (raises (fn #'tpl #'in)))))
)
