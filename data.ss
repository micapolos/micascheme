(library (data)
  (export data enum)
  (import
    (scheme)
    (syntax)
    (list-syntax)
    (binder)
    (identifier)
    (lets)
    (switch))

  (define-syntax data
    (lambda (stx)
      (syntax-case stx ()
        ((id (name field ... . list-field))
          (lets
            (fields (syntax->list #'(field ...)))
            (list-field-opt
              (and
                (not (null? (datum list-field)))
                #'list-field))
            (all-fields (append fields (or (and list-field-opt (list list-field-opt)) '())))
            (name-string (symbol->string (datum name)))
            (tmp (car (generate-temporaries '(tmp))))
            (record-name (identifier-append #'id #'% #'name))
            (make-name (identifier-append #'id #'make #'- #'name))
            (make-all-name (if list-field-opt make-name #'name))
            (rtd-name (identifier-append #'id #'name #'- #'rtd))
            (prefix-name (string-append name-string "-"))
            (predicate-name (identifier-append #'id #'name #'?))
            (accessors
              (map-with (field fields)
                (identifier-append #'id #'name #'- field)))
            (list-accessor-opt
              (and list-field-opt
                (identifier-append #'id #'name #'- list-field-opt)))
            (all-accessors
              (if list-accessor-opt
                (append accessors (list list-accessor-opt))
                accessors))
            (setters
              (map-with (field fields)
                (identifier-append #'id #'name #'- #'with #'- field)))
            (list-setter-opt
              (and list-field-opt
                (identifier-append #'id #'name #'- #'with #'- list-field-opt)))
            (all-setters
              (if list-setter-opt
                (append setters (list list-setter-opt))
                setters))
            #`(begin
              (define #,rtd-name
                (let ((#,tmp
                  (make-record-type #,name-string
                    (list
                      #,@(map
                        (lambda ($field)
                          #`(quote (immutable #,$field)))
                        all-fields)))))
                  (record-writer #,tmp
                    (lambda (record port wr)
                      (display "(" port)
                      (display #,name-string port)
                      #,@(map
                        (lambda (accessor)
                          #`(begin
                            (display " " port)
                            (wr (#,accessor record) port)))
                        accessors)
                      #,(if list-accessor-opt
                        #`(for-each
                            (lambda ($item)
                              (display " " port)
                              (wr $item port))
                            (#,list-accessor-opt record))
                        #`(begin (void)))
                      (display ")" port)))
                  (record-type-equal-procedure
                    #,tmp
                    (lambda (a b eq)
                      (and
                      #,@(map-with (field all-fields)
                        (lets
                          (fld (identifier-append #'id #'name #'- field))
                          #`(eq (#,fld a) (#,fld b)))))))
                  (record-type-hash-procedure
                    #,tmp
                    (lambda (a hash)
                      (+
                      #,@(map-with (field all-fields)
                        (lets
                          (fld (identifier-append #'id #'name #'- field))
                          #`(hash (#,fld a)))))))
                  #,tmp))
              #,(if list-field-opt
                #`(begin
                  (define (name field ... . list-field-opt)
                    ((record-constructor #,rtd-name) field ... list-field-opt))
                  (define #,make-name
                    (record-constructor #,rtd-name)))
                #`(begin
                  (define name
                    (record-constructor #,rtd-name))))
              (define #,predicate-name
                (record-predicate #,rtd-name))
              (define-binder name
                (lambda ($record $fn)
                  ($fn
                    #,@(map-with (accessor all-accessors)
                      #`(#,accessor $record)))))
              #,@(map-with
                (index (iota (length all-fields)))
                (field all-fields)
                #`(define #,(identifier-append #'id #'name #'- field)
                  (record-accessor #,rtd-name #,index)))
              #,@(map-with
                ($setter-index (iota (length all-setters)))
                ($setter all-setters)
                #`(define (#,$setter $record $value)
                  (#,make-all-name
                    #,@(map-with
                      ($accessor-index (iota (length all-accessors)))
                      ($accessor all-accessors)
                      (cond
                        ((= $setter-index $accessor-index) #'$value)
                        (else #`(#,$accessor $record)))))))))))))

  (define-syntax (enum $syntax)
    (syntax-case $syntax ()
      ((_ ($name $item ...))
        (identifiers? #`($name $item ...))
        (lets
          ($name #'$name)
          ($name-string (symbol->string (syntax->datum $name)))
          ($record-name
            (build-identifier
              ($string $name)
              (string-append $string "%")))
          ($name-prefix (string-append $name-string"-"))
          ($name-predicate
            (build-identifier
              ($string $name)
              (string-append $string "?")))
          ($name-switch
            (build-identifier
              ($string $name)
              (string-append $string "-switch")))
          ($name-body
            (build-identifier
              ($string $name)
              (string-append $string "-body")))
          ($rtd-tmp (car (generate-temporaries '(rtd))))
          ($name-tmp (car (generate-temporaries '(name))))
          ($case-tmp (car (generate-temporaries '(case))))
          ($dots (datum->syntax #'+ '...))
          #`(begin
            (define #,$record-name
              (let ((#,$rtd-tmp
                (make-record-type #,$name-string
                  (list '(immutable body)))))
                (record-writer #,$rtd-tmp
                  (record-pretty-writer #,$rtd-tmp #,$name-string))
                (record-type-equal-procedure
                  #,$rtd-tmp
                  (lambda (a b eq)
                    (eq (#,$name-body a) (#,$name-body b))))
                (record-type-hash-procedure
                  #,$rtd-tmp
                  (lambda (a hash)
                    (hash (#,$name-body a))))
                #,$rtd-tmp))
            (define #,$name
              (record-constructor #,$record-name))
            (define #,$name-predicate
              (record-predicate #,$record-name))
            (define #,$name-body
              (record-accessor #,$record-name 0))
            (define-rule-syntax (#,$name-switch #,$name-tmp #,$case-tmp #,$dots)
              (switch (#,$name-body #,$name-tmp)
                #,$case-tmp #,$dots)))))
      ((_ ($name ($data-name . $data-body) ...))
        (identifier? #'$name)
        #'(begin
          (enum ($name $data-name ...))
          (data ($data-name . $data-body))
          ...))))

  (define (record-pretty-writer rtd name)
    (lambda (record port wr)
      (let ((size (vector-length (record-type-field-names rtd))))
        (display "(" port)
        (display name port)
        (do
          ((i 0 (+ i 1)))
          ((= i size) (void))
          (display " " port)
          (wr ((record-accessor rtd i) record) port))
        (display ")" port))))
)
