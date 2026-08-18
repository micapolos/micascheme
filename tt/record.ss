(library (tt record)
  (export
    make-record
    define-record-constructor
    define-record-accessor
    define-union-constructor
    define-union-matcher
    (rename
      (%record-constructor record-constructor)
      (%record-accessor record-accessor)
      (%union-constructor union-constructor)
      (%union-matcher union-matcher)
      (%define-record define-record)))
  (import
    (scheme)
    (lets)
    (list-syntax)
    (syntax)
    (identifier)
    (procedure)
    (throw)
    (tt primitive)
    (tt type)
    (tt compiler)
    (prefix (tt lang) %))

  (%define-syntax make-record
    (lambda ($syntax)
      (syntax-case $syntax ()
        ((_ type . args)
          #`(%unchecked type
            #,(syntax-case #'args ()
              (() #''())
              ((x) #'x)
              ((x y) #'(cons x y))
              ((x ...) #'(vector x ...))))))))

  (%define-syntax %record-constructor
    (lambda ($syntax)
      (syntax-case $syntax (%lambda %pi)
        ((_ (%lambda (param ...) (%pi (field ...) record)))
          (for-all identifier? #'(param ...))
          #`(%unchecked
            (%lambda (param ...) (%pi (field ...) record))
            #,(case (length #'(field ...))
              ((0) #'(lambda () '()))
              ((1) #'(lambda (x) x))
              ((2) #'cons)
              (else #'vector)))))))

  (%define-syntax %record-accessor
    (lambda ($syntax)
      (syntax-case $syntax (%lambda %pi)
        ((_ index arity (%lambda (param ...) (%pi (record) field)))
          #`(%unchecked
            (%lambda (param ...) (%pi (record) field))
            #,(case (datum arity)
              ((1) #'identity)
              ((2) (if (zero? (datum index)) #'car #'cdr))
              (else #'(lambda (v) (vector-ref v index)))))))))

  (%define-syntax %union-constructor
    (lambda ($syntax)
      (syntax-case $syntax (%lambda %pi)
        ((_ index arity (%lambda (param ...) (%pi (option) union)))
          #`(%unchecked
            (%lambda (param ...) (%pi (option) union))
            #,(case (datum arity)
              ((1) #'identity)
              ((2) #`(lambda (x) (cons #,(literal->syntax (zero? (datum index))) x)))
              (else #`(lambda (x) (cons index x)))))))))

  (%define-syntax %union-matcher
    (lambda ($syntax)
      (syntax-case $syntax (%lambda %pi)
        ((_ (%lambda (param ... result) (%pi (union (%pi (option) r1) ...) r2)))
          #`(%unchecked
            (%lambda (param ... result) (%pi (union (%pi (option) r1) ...) r2))
            #,(case (length #'(r1 ...))
              ((1)
                #'(lambda (x a) (a x)))
              ((2)
                #'(lambda (x a b)
                  ((if (car x) a b) (cdr x))))
              (else
                (lets
                  ($tmps (generate-temporaries #'(r1 ...)))
                  #`(lambda (x #,@$tmps)
                    (case (car x)
                      #,@(map-with
                        ($tmp $tmps)
                        ($index (iota (length $tmps)))
                        #`((#,$index) (#,$tmp (cdr x))))))))))))))

  (define-syntax (define-record-constructor $syntax)
    (syntax-case $syntax (%lambda %pi)
      ((_ id (%lambda (param ...) (%pi (field ...) record)))
        (for-all identifier? #'(param ... id))
        #`(%define id
          (%record-constructor
            (%lambda (param ...) (%pi (field ...) record)))))))

  (define-syntax (define-record-accessor $syntax)
    (syntax-case $syntax (%lambda %pi)
      ((_ id index arity (%lambda (param ...) (%pi (record) field)))
        #`(%define id
          (%record-accessor index arity
            (%lambda (param ...) (%pi (record) field)))))))

  (define-syntax (define-union-constructor $syntax)
    (syntax-case $syntax (%lambda %pi)
      ((_ id index arity (%lambda (param ...) (%pi (option) union)))
        #`(%define id
          (%union-constructor index arity
            (%lambda (param ...) (%pi (option) union)))))))

  (define-syntax (define-union-matcher $syntax)
    (syntax-case $syntax (%lambda %pi)
      ((_ id (%lambda (param ... result) (%pi (union (%pi (option) r1) ...) r2)))
        #`(%define id
          (%union-matcher (%lambda (param ... result)
            (%pi (union (%pi (option) r1) ...) r2)))))))

  (define-syntax (%define-record $syntax)
    (lambda ($lookup)
      (syntax-case $syntax (%lambda)
        ((_ (id (%lambda t ...) (accessor-id field-type) ...))
          (for-all identifier? #'(id t ... accessor-id ...))
          (lets
            ($arity (length #'(accessor-id ...)))
            #`(begin
              (%define-class (id t ...))
              (define-record-constructor id
                (%lambda (t ...) (%pi (field-type ...) (id t ...))))
              #,@(map-with
                ($accessor-id #'(accessor-id ...))
                ($index (iota (length #'(accessor-id ...))))
                ($field-type #'(field-type ...))
                #`(define-record-accessor #,$accessor-id #,$index #,$arity
                  (%lambda (t ...) (%pi ((id t ...)) #,$field-type)))))))
        ((_ (id . x))
          (identifier? #'id)
          #`(%define-record (id (%lambda) . x))))))
)
