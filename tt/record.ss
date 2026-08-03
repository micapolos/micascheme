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
    (tt hoas-compiler)
    (prefix (tt lang) %))

  (define-syntax (%define-record $syntax)
    (lambda ($lookup)
      (syntax-case $syntax (%forall)
        ((_ (id (%forall t ...) (field-id field-type) ...))
          (for-all identifier? #'(id field-id ...))
          (lets
            ($accessor-ids
              (map
                (lambda ($field-id)
                  (identifier-append #'id #'id #'- $field-id))
                #'(field-id ...)))
            ($accessor-syntaxes
              (map
                (lambda ($index)
                  #`(lambda ($vector)
                    (vector-ref $vector
                      #,(literal->syntax $index))))
                (iota (length #'(field-id ...)))))
            #`(begin
              (%define-class (id t ...))
              (%define id
                (%unchecked
                  (%forall (t ...) (%pi (field-type ...) (id t ...)))
                  vector))
              #,@(map
                (lambda ($accessor-id $field-type $accessor-syntax)
                  #`(%define #,$accessor-id
                    (%unchecked
                      (%forall (t ...) (%pi ((id t ...)) #,$field-type))
                      #,$accessor-syntax)))
                $accessor-ids
                #'(field-type ...)
                $accessor-syntaxes))))
        ((_ (id . x))
          #`(%define-record (id (%forall) . x))))))
)
