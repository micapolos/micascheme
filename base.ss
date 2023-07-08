(library (base)
  (export 
    bind bind-true bind*
    check
    data partial
    define-aux-keyword
    obj=? record=? pair=? vector=? box=?
    displayln writeln
    fold-indices indices iterate
    find-index
    string+
    list-set
    switch
    unpair
    associ
    map-find-indexed
    map-indexed list-indexed
    indexed indexed? indexed-value indexed-index
    throw)

  (import 
    (chezscheme))

  (define (displayln x)
    (display x)
    (newline))

  (define (writeln x)
    (write x)
    (newline))

  (define string+ string-append)

  (define (partial $proc . $partial-args)
    (lambda $args
      (apply $proc (append $partial-args $args))))

  (define-syntax define-aux-keyword
    (syntax-rules ()
      ((_ aux)
        (define-syntax aux
          (lambda (stx)
            (syntax-error stx "misplaced aux keyword"))))))

  (define-syntax switch
    (lambda (stx) 
      (syntax-case stx (else)
        ((_ expr ((pred var) body ...) ... ((else else-var) else-body ...))
          (let ((tmp (car (generate-temporaries `(tmp)))))
            #`(let ((#,tmp expr))
              (cond
                ((pred #,tmp)
                  (let ((var #,tmp)) body ...)) ...
                (else 
                  (let ((else-var #,tmp)) else-body ...))))))
        ((_ expr ((pred var) body ...) ...)
          (let ((tmp (car (generate-temporaries `(tmp)))))
            #`(let ((#,tmp expr))
              (cond
                ((pred #,tmp)
                  (let ((var #,tmp)) body ...)) ...)))))))

  (define (fold-indices-from fn folded size index)
    (cond
      ((= index size) folded)
      (else (fold-indices-from fn (fn folded index) size (+ index 1)))))

  (define (fold-indices f folded size)
    (fold-indices-from f folded size 0))

  (define (indices size)
    (reverse (fold-indices (lambda ($list $index) (cons $index $list)) `() size)))

  (define (map-indexed $proc $list)
    (map $proc (indices (length $list)) $list))

  (define (list-indexed $list)
    (map-indexed (lambda ($index $value) (indexed $value $index)) $list))

  (define (obj=? a b)
    (cond
      ((record? a) (and (record? b) (record=? a b)))
      ((string? a) (and (string? b) (string=? a b)))
      ((pair? a) (and (pair? b) (pair=? a b)))
      ((bytevector? a) (and (bytevector? b) (bytevector=? a b)))
      ((vector? a) (and (vector? b) (vector=? a b)))
      ((box? a) (and (box? b) (box=? a b)))
      (else (eqv? a b))))

  (define (pair=? pair-a pair-b)
    (and 
      (obj=? (car pair-a) (car pair-b))
      (obj=? (cdr pair-a) (cdr pair-b))))

  (define (box=? box-a box-b)
    (obj=? (unbox box-a) (unbox box-b)))

  (define (record=? a b)
    (let ((rtd-a (record-rtd a))
          (rtd-b (record-rtd b)))
      (and
        (eq? rtd-a rtd-b)
        (fold-indices
          (lambda (eq index)
            (let ((get (record-accessor rtd-a index)))
              (and eq (obj=? (get a) (get b)))))
          #t
          (vector-length (record-type-field-names rtd-a))))))

  (define (vector=? vector-a vector-b)
    (let ((length-a (vector-length vector-a))
          (length-b (vector-length vector-b)))
      (and (= length-a length-b)
        (fold-indices
          (lambda (eq index)
            (and eq (obj=? (vector-ref vector-a index) (vector-ref vector-b index))))
          #t
          length-a))))

  (define-syntax data
    (lambda (stx)
      (syntax-case stx ()
        ((_ (name field ...))
          (let* ((name-string (symbol->string (syntax->datum #`name)))
                 (record-name (datum->syntax #`name (string->symbol (string-append "%" name-string))))
                 (rtd-name (datum->syntax #`name (string->symbol (string-append name-string "-rtd"))))
                 (prefix-name (string-append name-string "-"))
                 (predicate-name (datum->syntax #`name (string->symbol (string-append name-string "?")))))
            #`(begin
              (define-record 
                #,record-name
                ((immutable field) ...)
                ()
                ((constructor name) 
                 (prefix #,prefix-name) 
                 (predicate #,predicate-name)))
              (define #,rtd-name
                (let ((td (type-descriptor #,record-name)))
                  (record-writer td (record-pretty-writer td #,name-string))
                  td))))))))

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

  (define-syntax unpair
    (lambda (stx)
      (syntax-case stx ()
        ((_ expr lhs rhs body ...)
          (let ((tmp (car (generate-temporaries `(tmp)))))
            #`(let ((#,tmp expr))
              (let ((lhs (car #,tmp))
                    (rhs (cdr #,tmp)))
                body ...)))))))

  (define-syntax throw
    (lambda (stx)
      (syntax-case stx ()
        ((_ name item ...) (identifier? #`name)
          #`(error #f (format "~s" (list (quote name) #,@(syntax->list #`(item ...)))))))))

  (define-syntax bind
    (lambda (stx)
      (syntax-case stx ()
        ((_ (var expr) body ...)
          #`(let ((var expr)) body ...)))))

  (define-syntax bind*
    (lambda (stx)
      (syntax-case stx ()
        ((_ decl ... body)
          #`(let* (decl ...) body)))))

  (define-syntax bind-true
    (lambda (stx)
      (syntax-case stx ()
        ((_ (var expr) body ...)
          #`(bind (var expr) 
            (cond (var body ...) (else #f)))))))

  (define (list-set $list $index $obj)
    (if (> $index 0)
      (cons (car $list) (list-set (cdr $list) (- $index 1) $obj))
      (cons $obj (cdr $list))))

  (define (associ $list $index $obj)
    (cond
      ((null? $list) #f)
      ((equal? (caar $list) $obj) (cons $index (cdar $list)))
      (else (associ (cdr $list) (+ $index 1) $obj))))

  (define-syntax check
    (lambda (stx)
      (syntax-case stx (not)
        ((_ (not (pred arg ...)))
          (let* ((args (syntax->list #`(arg ...)))
                 (tmps (generate-temporaries #`(arg ...)))
                 (let-cases (map (lambda (tmp arg) #`(#,tmp #,arg)) tmps args))
                 (ann (syntax->annotation stx))
                 (source (annotation-source ann))
                 (ann-string 
                  (if ann 
                    (format " source: ~a (<line>:<char>)\n" (source-file-descriptor-path (source-object-sfd source)))
                    "")))
            #`(let (#,@let-cases)
              (or (not (pred #,@tmps))
                (error `check 
                  (format "\n~a   expr: ~s\n  value: ~s\n"
                    #,ann-string
                    (quote (not (pred arg ...)))
                    (list (quote not) (list (quote pred) #,@tmps))))))))
        ((_ (pred arg ...))
          (let* ((args (syntax->list #`(arg ...)))
                 (tmps (generate-temporaries #`(arg ...)))
                 (let-cases (map (lambda (tmp arg) #`(#,tmp #,arg)) tmps args))
                 (ann (syntax->annotation stx))
                 (source (annotation-source ann))
                 (ann-string 
                  (if ann 
                    (format " source: ~a (<line>:<char>)\n" (source-file-descriptor-path (source-object-sfd source)))
                    "")))
            #`(let (#,@let-cases)
              (or (pred #,@tmps)
                (error `check 
                  (format "\n~a   expr: ~s\n  value: ~s\n"
                    #,ann-string
                    (quote (pred arg ...))
                    (list (quote pred) #,@tmps))))))))))

  ; --------------------------------------

  (data (indexed value index))

  (define (map-find-indexed $proc $list)
    (map-find-indexed+ $proc $list 0))

  (define (map-find-indexed+ $proc $list $index)
    (and
      (not (null? $list))
      (bind ($mapped ($proc (car $list)))
        (if $mapped
          (indexed $mapped $index)
          (map-find-indexed+ $proc (cdr $list) (+ $index 1))))))

  (define (find-index $proc $list)
    (bind-true ($indexed (map-find-indexed $proc $list))
      (indexed-index $indexed)))

  (define (iterate $proc $item $count)
    (cond
      ((= $count 0) $item)
      (else (iterate $proc ($proc $item) (- $count 1)))))

  ; --------------------------------------

  (data (foo a b))
  (data (bar a b))

  ; ---------------------------------------

  (assert
    (equal?
      (switch (string-append "a" "b")
        ((string? s) (cons `string s))
        ((number? n) (cons `number n)))
      (cons `string "ab")))

  (assert
    (equal?
      (switch (+ 1 2)
        ((string? s) (cons `string s))
        ((number? n) (cons `number n)))
      (cons `number 3)))

  (assert
    (equal?
      (switch (not #t)
        ((string? s) (cons `string s))
        ((number? n) (cons `number n)))
      (void)))

  (assert
    (equal?
      (switch (string-append "a" "b")
        ((string? s) (cons `string s))
        ((number? n) (cons `number n))
        ((else e) (cons `other e)))
      (cons `string "ab")))

  (assert
    (equal?
      (switch (+ 1 2)
        ((string? s) (cons `string s))
        ((number? n) (cons `number n))
        ((else e) (cons `other e)))
      (cons `number 3)))

  (assert
    (equal?
      (switch (not #t)
        ((string? s) (cons `string s))
        ((number? n) (cons `number n))
        ((else e) (cons `other e)))
      (cons `other #f)))

  (assert (record=? (foo 1 2) (foo 1 2)))

  (assert (not (record=? (foo 1 2) (foo 1 3))))
  (assert (not (record=? (foo 1 2) (bar 1 2))))

  (assert (= (unpair (cons 3 2) l r (- l r)) 1))
)
