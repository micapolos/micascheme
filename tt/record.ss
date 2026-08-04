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
        ((_ (id (%forall t ...) (accessor-id field-type) ...))
          (for-all identifier? #'(id t ... accessor-id ...))
          (lets
            ($arity (length #'(accessor-id ...)))
            #`(begin
              (%define-class (id t ...))
              (define-record-constructor id
                (%forall (t ...) (%pi (field-type ...) (id t ...))))
              #,@(map-with
                ($accessor-id #'(accessor-id ...))
                ($index (iota (length #'(accessor-id ...))))
                ($field-type #'(field-type ...))
                #`(define-record-accessor #,$accessor-id #,$index #,$arity
                  (%forall (t ...) (%pi ((id t ...)) #,$field-type)))))))
        ((_ (id . x))
          (identifier? #'id)
          #`(%define-record (id (%forall) . x))))))
)
