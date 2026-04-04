(library (leo record)
  (export
    record
    leo-define-record)
  (import
    (except (scheme) predicate syntax-error)
    (syntax)
    (procedure)
    (lets)
    (keyword)
    (system)
    (leo maker)
    (leo predicate)
    (leo getter-leo)
    (leo setter!)
    (leo field-spec)
    (leo syntax-error))

  (define-keyword record)

  (define-syntax (leo-define-record $syntax)
    (syntax-case $syntax ()
      ((_ id)
        (keyword? id)
        #'(define-record (id)))
      ((_ (id field-spec ...))
        (keyword? id)
        (lets
          (name (symbol->string (datum id)))
          (field-specs (map field-spec-normalize #'(field-spec ...)))
          (field-count (length field-specs))
          (field-classes (map car field-specs))
          (field-types (map cadr field-specs))
          (field-names (map caddr field-specs))
          (record-type-id (car (generate-temporaries '(record-type))))
          (make-id (car (generate-temporaries '(make))))
          (predicate-id (car (generate-temporaries '(is?))))
          (field-ids (generate-temporaries field-names))
          #`(begin
            (define-values
              (#,record-type-id #,make-id #,predicate-id #,@field-ids)
              (lets
                (record-type
                  (make-record-type
                    #,(literal->syntax name)
                    '(#,@(map literal->syntax field-specs))))
                (apply values
                  record-type
                  (record-constructor record-type)
                  (record-predicate record-type)
                  (map
                    (partial record-accessor record-type)
                    (iota #,(literal->syntax field-count))))))
            (define-keyword id)
            (define-property id maker #'#,make-id)
            (define-property id predicate #'#,predicate-id)
            (define-property id getter
              (lambda (field-id)
                (lets
                  (field-name (syntax->datum field-id))
                  (case field-name
                    #,@(map
                      (lambda (field-name field-id)
                        #`(
                          (#,(literal->syntax field-name))
                          #'#,field-id))
                      field-names
                      field-ids)
                    (else
                      (syntax-error field-id
                        `(undefined (id ,field-name)))))))))))))
  ; TODO: hash and equal procedures
)
