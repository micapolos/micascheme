(library (tt record)
  (export
    define-record-constructor
    define-record-accessor
    define-union-constructor
    define-union-matcher
    (rename
      (%define-record define-record)))
  (import
    (scheme)
    (lets)
    (list-syntax)
    (syntax)
    (identifier)
    (procedure)
    (tt primitive)
    (tt type)
    (tt hoas-compiler)
    (prefix (tt lang) %))

  (define-syntax (define-record-constructor $syntax)
    (syntax-case $syntax (%forall %pi)
      ((_ id (%forall (param ...) (%pi (field ...) record)))
        (for-all identifier? #'(param ... id))
        #`(%define id
          (%unchecked
            (%forall (param ...) (%pi (field ...) record))
            #,(case (length #'(field ...))
              ((0) #'(lambda () '()))
              ((1) #'(lambda (x) x))
              ((2) #'cons)
              (else #'vector)))))))

  (define-syntax (define-record-accessor $syntax)
    (syntax-case $syntax (%forall %pi)
      ((_ id index arity (%forall (param ...) (%pi (record) field)))
        #`(%define id
          (%unchecked
            (%forall (param ...) (%pi (record) field))
            #,(case (datum arity)
              ((1) #'identity)
              ((2) (if (zero? (datum index)) #'car #'cdr))
              (else #'(lambda (v) (vector-ref v index)))))))))

  (define-syntax (define-union-constructor $syntax)
    (syntax-case $syntax (%forall %pi)
      ((_ id index arity (%forall (param ...) (%pi (option) union)))
        #`(%define id
          (%unchecked
            (%forall (param ...) (%pi (option) union))
            #,(case (datum arity)
              ((1) #'identity)
              ((2) #`(lambda (x) (cons #,(literal->syntax (zero? (datum index))) x)))
              (else #`(lambda (x) (cons index x)))))))))

  (define-syntax (define-union-matcher $syntax)
    (syntax-case $syntax (%forall %pi)
      ((_ id (%forall (param ... result) (%pi (union (%pi (option) r1) ...) r2)))
        #`(%define id
          (%unchecked
            (%forall (param ... result) (%pi (union (%pi (option) r1) ...) r2))
            #,(case (length #'(option ...))
              ((1)
                #'(lambda (x a) (a x)))
              ((2)
                #'(lambda (x a b)
                  ((if (car x) a b) (cdr x))))
              (else
                (lets
                  ($tmps (generate-temporaries #'(option ...)))
                  #`(lambda (x #,@$tmps)
                    (case (car x)
                      #,@(map-with
                        ($tmp $tmps)
                        ($index (iota (length $tmps)))
                        #`((#,$index) (#,$tmp (cdr x))))))))))))))

  (define-syntax (%define-record $syntax)
    (lambda ($lookup)
      (syntax-case $syntax (%forall)
        ((_ (id (%forall t ...) (field-id field-type) ...))
          (for-all identifier? #'(id t ... field-id ...))
          (lets
            ($arity (length #'(field-id ...)))
            ($accessor-ids
              (map
                (lambda ($field-id)
                  (identifier-append #'id #'id #'- $field-id))
                #'(field-id ...)))
            ($accessor-syntaxes
              (map
                (lambda ($index)
                  #`(lambda (x)
                    #,(case $arity
                      ((1) #'x)
                      ((2) #`(#,(if (zero? $index) #'car #'cdr) x))
                      (else #`(vector-ref x #,(literal->syntax $index))))))
                (iota $arity)))
            #`(begin
              (%define-class (id t ...))
              (define-record-constructor id
                (%forall (t ...) (%pi (field-type ...) (id t ...))))
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
          (identifier? #'id)
          #`(%define-record (id (%forall) . x)))
        ((_ id)
          #`(begin
              (%define-class id)
              (%define id (%unchecked id '())))))))
)
