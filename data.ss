(library (data)
  (export data enum)
  (import
    (scheme)
    (syntax)
    (binder)
    (identifier)
    (lets)
    (switch))

  (define-syntax data
    (lambda (stx)
      (syntax-case stx ()
        ((_ (name field ... . list-field))
          (lets
            (fields (syntax->list #'(field ...)))
            (list-field-opt
              (and
                (not (null? (syntax->datum #'list-field)))
                #'list-field))
            (all-fields (append fields (or (and list-field-opt (list list-field-opt)) '())))
            (name-string (symbol->string (syntax->datum #`name)))
            (tmp (car (generate-temporaries '(tmp))))
            (record-name (build-identifier ($string #`name) (string-append "%" $string)))
            (rtd-name (build-identifier ($string #`name) (string-append $string "-rtd")))
            (prefix-name (string-append name-string "-"))
            (predicate-name (build-identifier ($string #`name) (string-append $string "?")))
            (accessors
              (map
                (lambda ($field)
                  (build-identifier ($string $field)
                    (string-append name-string "-" $string)))
                fields))
            (list-accessor-opt
              (and list-field-opt
                (build-identifier ($string list-field-opt)
                  (string-append name-string "-" $string))))
            (all-accessors
              (if list-accessor-opt
                (append accessors (list list-accessor-opt))
                accessors))
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
                      #,@(map
                        (lambda (field)
                          (lets
                            (fld (build-identifier (s field) (string-append name-string "-" s)))
                            #`(eq (#,fld a) (#,fld b))))
                        all-fields))))
                  (record-type-hash-procedure
                    #,tmp
                    (lambda (a hash)
                      (+
                      #,@(map
                        (lambda (field)
                          (lets
                            (fld (build-identifier (s field) (string-append name-string "-" s)))
                            #`(hash (#,fld a))))
                        all-fields))))
                  #,tmp))
              #,(if list-field-opt
                #`(begin
                  (define (name field ... . list-field-opt)
                    ((record-constructor #,rtd-name) field ... list-field-opt))
                  (define-accessors (name #,@accessors . #,list-accessor-opt)))
                #`(begin
                  (define name
                    (record-constructor #,rtd-name))
                  (define-accessors (name #,@accessors))))
              (define #,predicate-name
                (record-predicate #,rtd-name))
              #,@(map
                (lambda (index f)
                  #`(define #,(build-identifier (s f) (string-append prefix-name s))
                    (record-accessor #,rtd-name #,index)))
                (iota (length all-fields))
                all-fields)))))))

  (define-syntax enum
    (lambda ($syntax)
      (syntax-case $syntax ()
        ((_ ($name $item ...))
          (and
            (identifier? #`$name)
            (for-all identifier? (syntax->list #`($item ...))))
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
              (define-syntax-rule (#,$name-switch #,$name-tmp #,$case-tmp #,$dots)
                (switch (#,$name-body #,$name-tmp)
                  #,$case-tmp #,$dots))))))))

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
