(library (tt record)
  (export
    (rename
      (%define-record define-record)))
  (import
    (scheme)
    (lets)
    (syntax)
    (identifier)
    (procedure)
    (tt primitive)
    (tt type)
    (tt hoas-compiler))

  (define-syntax (%define-record $syntax)
    (lambda ($lookup)
      (syntax-case $syntax ()
        ((_ (id (field-id field-type) ...))
          (for-all identifier? #'(id field-id ...))
          (lets
            ($declaration
              (generate-declaration
                (symbol->string (datum id))
                0))
            ($class (class $declaration (list)))
            ($field-types (map (partial compile-type $lookup) #'(field-type ...)))
            ($accessor-ids
              (map
                (lambda ($field-id)
                  (identifier-append #'id #'id #'- $field-id))
                #'(field-id ...)))
            ($accessor-types
              (map
                (lambda ($type)
                  (arrow (list $class) #f $type))
                $field-types))
            ($accessor-syntaxes
              (map
                (lambda ($index)
                  #`(lambda ($vector)
                    (vector-ref $vector
                      #,(literal->syntax $index))))
                (iota (length $field-types))))
            ($typed-accessors
              (map typed $accessor-types $accessor-syntaxes))
            #`(begin
              (define-syntax id (make-compile-time-value #t))
              (define-property id declaration
                #,(declaration->syntax $declaration))
              (define-property id typed
                #,(typed->syntax
                  (typed
                    (arrow $field-types #f (class $declaration (list)))
                    #'vector)))
              #,@(map
                (lambda ($accessor-id $field-type $accessor-syntax)
                  #`(define-syntax
                    #,$accessor-id
                    (make-compile-time-value
                      #,(typed->syntax
                        (typed
                          (arrow (list (class $declaration (list))) #f $field-type)
                          $accessor-syntax)))))
                $accessor-ids
                $field-types
                $accessor-syntaxes)))))))
)
