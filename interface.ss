(library (interface)
  (export define-interface)
  (import
    (scheme)
    (lets)
    (list-syntax))

  (define-syntax (define-interface $syntax)
    (syntax-case $syntax ()
      ((_ (id id?) (proc-id . params) ...)
        (lets
          ($field-ids (generate-temporaries #'(proc-id ...)))
          ($accessor-ids (generate-temporaries #'(proc-id ...)))
          #`(begin
            (define-record-type (foo id id?)
              (fields
                #,@(map-with
                  ($field-id $field-ids)
                  ($accessor-id $accessor-ids)
                  #`(immutable #,$field-id #,$accessor-id))))
            #,@(map-with
              ($accessor-id $accessor-ids)
              ($proc-id #'(proc-id ...))
              ($params #'(params ...))
              #`(define (#,$proc-id $x . #,$params)
                ((#,$accessor-id $x) . #,$params))))))))
)
