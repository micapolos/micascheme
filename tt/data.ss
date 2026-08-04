(library (tt data-macros)
  (export)
  (import
    (scheme)
    (list)
    (list-syntax)
    (prefix (tt lang) %))

  (define-syntax (define-record-constructor $syntax)
    (syntax-case $syntax ()
      ((_ (id t ...) (p ...) r)
        #`(define id
          (unchecked
            (forall (t ...) (pi (p ...) r))
            #,(case (length #'(p ...))
              ((0) #'(lambda () '()))
              ((1) #'(lambda (x) x))
              ((2) %cons)
              (else %vector)))))))

  (define-syntax (define-record-accessor $syntax)
    (syntax-case $syntax ()
      ((_ (data t ...) (id in arity index) out)
        #(define id
          (unchecked
            (forall (t ...) (pi (in) out))
            #,(case (datum arity)
              ((1) #'(lambda (x) x))
              ((2) (if (zero? (datum index) #'car #'cdr)))
              (else #`(lambda (v) (vector-ref v index)))))))))

  (define-syntax (define-record-accessors $syntax)
    (syntax-case $syntax ()
      ((_ (data t ...) (id in out) ...)
        (lets
          ($arity (length #'(id ...)))
          #,@(map-with
            ($index (iota #'(id ...)))
            ($id #'(id ...))
            ($in #'(in ...))
            ($out #'(out ...))
            #`(define-record-accessor
              (data t ...)
              (#,$id
                #,$in
                #,(literal->syntax $arity)
                #,(literal->syntax $index))
              #,$out))))))

  (define (define-match $syntax)
    (syntax-case $syntax ()
      ((id ))

  (define (transform-data-component $syntax)
    (syntax-case $syntax (%of)
      ((data (constructor (accessor type)))
        (identifier? #'data)
        #`(begin
          (define constructor (%unchecked (pi (type) data) identity))
          (define accessor (%unchecked (pi (data) type) identity))))
      ((data (constructor (accessor-1 type-1) (accessor-2 type-2)))
        (identifier? #'data)
        #`(begin
          (%define constructor (%unchecked (pi (type-1 type-2) data) cons))
          (%define accessor-1 (%unchecked (pi (data) type-1) car))
          (%define accessor-2 (%unchecked (pi (data) type-2) cdr))))
      ((data (constructor (accessor type) ...))
        (identifier? #'data)
        #`(begin
          (%define constructor (%unchecked (pi (type ...) data) vector))
          #,@(map-with
            ($accessor #'(accessor ...))
            ($type #'(type ...))
            ($index (map literal->syntax (iota (length #'(type ...)))))
            #`(%define #,$accessor
              (%unchecked
                (pi (data) #,$type)
                (vector-accessor #,$index))))))))

  (define (transform-data $syntax)
    (syntax-case $syntax ()
      ((id component)
        #`(begin
          (%define-class id)
          #,@(map-with
            ($component #'(component ...))
            (transform-data-component #`(id #,$component)))))))
)
