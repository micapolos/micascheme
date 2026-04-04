(library (leo record)
  (export record)
  (import
    (except (scheme) predicate syntax-error define)
    (syntax)
    (syntaxes)
    (procedure)
    (lets)
    (keyword)
    (system)
    (identifier)
    (list)
    (list-syntax)
    (leo define)
    (leo maker)
    (leo predicate)
    (leo getter-leo)
    (leo setter!)
    (leo definer)
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
          (field-indices (iota field-count))
          (mutable-field-indices
            (?filter
              (map-with
                (field-class field-classes)
                (field-index field-indices)
                (and (symbol=? field-class 'mutable) field-index))))
          (mutable-field-names
            (map (partial list-ref field-names) mutable-field-indices))
          (field-ids (map (partial datum->syntax #'id) field-names))
          (mutable-field-ids (map (partial datum->syntax #'id) mutable-field-names))
          (getter-ids
            (map-with
              (field-id field-ids)
              (identifier-append #'id #'id #'- field-id)))
          (setter-ids
            (map-with
              (field-id mutable-field-ids)
              (identifier-append #'id #'id #'- field-id #'- #'set!)))
          (variable-ids (generate-temporaries field-ids))
          (params (map syntax-append field-ids variable-ids))
          (fields-id (apply identifier-append #'id (intercalate field-ids #'-)))
          (record-type-id (identifier-append #'id #'id #'- #'type))
          (make-id (identifier-append #'id fields-id #'-> #'id))
          (predicate-id (identifier-append #'id #'id #'?))
          #`(begin
            (define-values
              (#,record-type-id #,make-id #,predicate-id #,@getter-ids #,@setter-ids)
              (lets
                (record-type
                  (make-record-type
                    #,(literal->syntax name)
                    '(#,@(map literal->syntax field-specs))))
                (values
                  record-type
                  (record-constructor record-type)
                  (record-predicate record-type)
                  #,@(map-with (index field-indices)
                    #`(record-accessor record-type #,(literal->syntax index)))
                  #,@(map-with (index mutable-field-indices)
                    #`(record-mutator record-type #,(literal->syntax index))))))
            (define-rules-syntax
              (keywords #,@field-ids set!)
              ((id #,@params)
                (#,make-id #,@variable-ids))
              #,@(map-with
                (field-id field-ids)
                (getter-id getter-ids)
                #`((id (#,field-id rec))
                  (#,getter-id rec)))
              #,@(map-with
                (field-id mutable-field-ids)
                (setter-id setter-ids)
                #`((id (set! (#,field-id rec x)))
                  (#,setter-id rec x)))))))))

  ; TODO: hash and equal procedures

  (define
    (definer
      (record
        (lambda ($syntax)
          #`(leo-define-record . #,$syntax)))))
)
