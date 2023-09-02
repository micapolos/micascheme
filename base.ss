(library (base)
  (export 
    identity

    list-get
    list-get-overflow list-get-overflow? list-get-overflow-index

    failure failure? failure-value failure!

    pure
    from
    single? single
    script
    and-lets lets
    opt-apply
    works?
    check checking? test-all
    data partial
    define-aux-keyword define-syntax-rule
    obj=? record=? pair=? vector=? box=?
    displayln writeln
    fold-indices indices iterate
    find-index
    list-set list-ref-opt
    switch
    unpair
    associ
    filter-map filter-opts
    map-find-indexed
    map-indexed list-indexed
    indexed indexed? indexed-value indexed-index
    throw

    stack push top pop

    generate-temporary
    build-identifier

    struct-constructor-syntax
    struct-accessor-syntax
    struct->datum-syntax
    struct-syntax

    one-of-constructor-syntax
    one-of-switch-syntax
    one-of->datum-syntax
    one-of-syntax)

  (import (scheme))

  (define identity (lambda (x) x))

  (define (displayln x) (display x) (newline))
  (define (writeln x) (write x) (newline))

  (define (works? expr) expr #t)

  (define (push $stack $item) (cons $item $stack))
  (define (top $stack) (car $stack))
  (define (pop $stack) (cdr $stack))

  (define-syntax stack
    (lambda ($syntax)
      (syntax-case $syntax ()
        ((_ $item ...) 
          #`(list #,@(reverse (syntax->list #`($item ...))))))))

  (define (single $list)
    (and (single? $list) (car $list)))

  (define (single? $list)
    (and
      (pair? $list) 
      (null? (cdr $list))))

  (define (generate-temporary $obj) 
    (if checking?
      (build-identifier ($string $obj) (string-append "$" $string))
      (car (generate-temporaries (list $obj)))))

  (define-syntax define-syntax-rule
    (syntax-rules ()
      ((_ (name param ...) body)
        (define-syntax name
          (syntax-rules ()
            ((_ param ...) body))))))

  (define-syntax-rule (build-identifier ($var $id) $body)
    (datum->syntax $id
      (string->symbol 
        (lets ($var (symbol->string (syntax->datum $id))) $body))))

  (define-syntax-rule (test-all file ...) 
    (begin
      (let () (load file)) ...))

  (define-syntax lets
    (lambda ($syntax)
      (syntax-case $syntax ()
        ((_ $decl $decls ... $result)
          (syntax-case #`$decl (do rec)
            ((($id ...) $expr)
              #`(let-values ((($id ...) $expr))
                (lets $decls ... $result)))
            (($id (rec $expr))
              #`(letrec (($id $expr))
                (lets $decls ... $result)))
            (($id $expr)
              #`(let (($id $expr))
                (lets $decls ... $result)))
            ($expr
              #`(begin $expr
              (lets $decls ... $result)))))
        ((_ $result) #`$result))))

  (define-syntax and-lets
    (syntax-rules ()
      ((_ body) body)
      ((_ (val expr) decl ... body)
        (let ((val expr)) 
          (and val (and-lets decl ... body))))))

  (define-syntax from
    (lambda ($syntax)
      (syntax-case $syntax ()
        ((_ $package-spec $identifier)
          #`(let ()
            (import-only (only $package-spec $identifier))
            $identifier)))))

  (define-syntax script
    (lambda ($syntax)
      (syntax-case $syntax ()
        ((_ $target $op1 $op2 ...)
          (syntax-case #`$op1 ()
            (($name $arg ...)
              #`(script ($name $target $arg ...) $op2 ...))))
        ((_ $target) #`$target))))

  (define (partial $proc . $partial-args)
    (lambda $args
      (apply $proc (append $partial-args $args))))

  (define-syntax-rule (define-aux-keyword aux)
    (define-syntax aux
      (lambda (stx)
        (syntax-error stx "misplaced aux keyword"))))

  (define-aux-keyword pure)

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

  (define (filter-map $proc . $list)
    (filter (lambda (x) x) (apply map (cons $proc $list))))

  (define (filter-opts $opts)
    (filter identity $opts))

  (define-syntax opt-apply
    (lambda ($syntax)
      (syntax-case $syntax ()
        ((_ $fn $spec ...)
          (lets
            ($pure-arg-pairs
              (map
                (lambda ($spec)
                  (syntax-case $spec (pure)
                    ((pure $arg) (cons #t #`$arg))
                    ($arg (cons #f #`$arg))))
                (syntax->list #`($spec ...))))
            ($pures (map car $pure-arg-pairs))
            ($args (map cdr $pure-arg-pairs))
            ($tmps (generate-temporaries $args))
            ($opt-tmps
              (filter (lambda (x) x)
                (map
                  (lambda ($pure $tmp) (and (not $pure) $tmp))
                  $pures $tmps)))
            #`(lets
              #,@(map (lambda ($tmp $arg) #`(#,$tmp #,$arg)) $tmps $args)
              (and #,@$opt-tmps ($fn #,@$tmps))))))))

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
          (lets
            (name-string (symbol->string (syntax->datum #`name)))
            (record-name (build-identifier ($string #`name) (string-append "%" $string)))
            (rtd-name (build-identifier ($string #`name) (string-append $string "-rtd")))
            (prefix-name (string-append name-string "-"))
            (predicate-name (build-identifier ($string #`name) (string-append $string "?")))
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

  (define (list-ref-opt $list $index)
    (and
      (pair? $list)
      (if (= $index 0)
        (car $list)
        (list-ref-opt (cdr $list) (- $index 1)))))

  (define (list-set $list $index $obj)
    (if (> $index 0)
      (cons (car $list) (list-set (cdr $list) (- $index 1) $obj))
      (cons $obj (cdr $list))))

  (define (associ $list $index $obj)
    (cond
      ((null? $list) #f)
      ((equal? (caar $list) $obj) (cons $index (cdar $list)))
      (else (associ (cdr $list) (+ $index 1) $obj))))

  (define checking? (make-thread-parameter #f))

  (define-syntax check
    (lambda (stx)
      (syntax-case stx (not)
        ((_ (not (pred arg ...)))
          (lets
            (args (syntax->list #`(arg ...)))
            (tmps (generate-temporaries #`(arg ...)))
            (let-cases (map (lambda (tmp arg) #`(#,tmp #,arg)) tmps args))
            (ann (syntax->annotation stx))
            (source (annotation-source ann))
            (ann-string 
              (if ann
                (format " source: ~a (~a:~a)\n"
                  (source-file-descriptor-path (source-object-sfd source))
                  (source-object-line source)
                  (source-object-column source))
                ""))
            #`(let (#,@let-cases)
              (or (parameterize ((checking? #t)) (not (pred #,@tmps)))
                (error `check 
                  (format "\n~a   expr: ~s\n  value: ~s\n"
                    #,ann-string
                    (quote (not (pred arg ...)))
                    (list (quote not) (list (quote pred) #,@tmps))))))))
        ((_ (pred arg ...))
          (lets
            (args (syntax->list #`(arg ...)))
            (tmps (generate-temporaries #`(arg ...)))
            (let-cases (map (lambda (tmp arg) #`(#,tmp #,arg)) tmps args))
            (ann (syntax->annotation stx))
            (source (annotation-source ann))
            (ann-string 
              (if ann
                (format " source: ~a (~a:~a)\n"
                  (source-file-descriptor-path (source-object-sfd source))
                  (source-object-line source)
                  (source-object-column source))
                ""))
            #`(let (#,@let-cases)
              (or (parameterize ((checking? #t)) (pred #,@tmps))
                (error `check 
                  (format "\n~a   expr: ~s\n  value: ~s\n"
                    #,ann-string
                    (quote (pred arg ...))
                    (list (quote pred) #,@tmps))))))))))

  ; --------------------------------------

  (data (failure value))

  (define-syntax-rule (failure! value)
    (failure (quote value)))

  ; --------------------------------------

  (data (indexed value index))

  (define (map-find-indexed $proc $list)
    (map-find-indexed+ $proc $list 0))

  (define (map-find-indexed+ $proc $list $index)
    (and
      (not (null? $list))
      (lets 
        ($mapped ($proc (car $list)))
        (if $mapped
          (indexed $mapped $index)
          (map-find-indexed+ $proc (cdr $list) (+ $index 1))))))

  (define (find-index $proc $list)
    (and-lets ($indexed (map-find-indexed $proc $list))
      (indexed-index $indexed)))

  (define (iterate $proc $item $count)
    (cond
      ((= $count 0) $item)
      (else (iterate $proc ($proc $item) (- $count 1)))))

  (data (list-get-overflow index))

  (define (list-get $list $index)
    (if (null? $list)
      (list-get-overflow $index)
      (if (= $index 0)
        (car $list)
        (list-get (cdr $list) (- $index 1)))))

  ; --------------------------------------

  (define (struct-constructor-syntax $name $fields)
    #`(define-syntax-rule (#,$name #,@$fields)
      #,(case (length $fields)
        ((0) #f)
        ((1) (car $fields))
        ((2) #`(cons #,(car $fields) #,(cadr $fields)))
        (else #`(vector #,@$fields)))))

  (define (struct-accessor-syntax $name $fields $index)
    (lets
      ($name-string (symbol->string (syntax->datum $name)))
      ($field (list-ref $fields $index))
      ($field-string (symbol->string (syntax->datum $field)))
      ($accessor-string (string-append $name-string "-" $field-string))
      ($accessor (datum->syntax $field (string->symbol $accessor-string)))
      (case (length $fields)
        ((1) 
          #`(define-syntax-rule (#,$accessor expr) expr))
        ((2) 
          #`(define-syntax-rule (#,$accessor expr)
            (#,(if (= $index 0) #`car #`cdr) expr)))
        (else
          #`(define-syntax-rule (#,$accessor expr) 
            (vector-ref expr #,$index))))))

  (define (struct->datum-syntax $name $fields)
    (lets
      ($name-string (symbol->string (syntax->datum $name)))
      ($name->datum-string (string-append $name-string "->datum"))
      ($name->datum (datum->syntax $name (string->symbol $name->datum-string)))
      ($field-strings (map symbol->string (map syntax->datum $fields)))
      ($accessor-strings (map (lambda ($field-string) (string-append $name-string "-" $field-string)) $field-strings))
      ($accessors (map (partial datum->syntax $name) (map string->symbol $accessor-strings)))
      ($datum-strings (map (lambda ($field-string) (string-append $field-string "->datum")) $field-strings))
      ($datums (map (partial datum->syntax $name) (map string->symbol $datum-strings)))
      ($tmp (generate-temporary #`expr))
      #`(define (#,$name->datum #,$tmp)
        (quasiquote
          (#,$name
            #,@(map 
              (lambda ($accessor $datum) #`(unquote (#,$datum (#,$accessor #,$tmp))))
              $accessors
              $datums))))))

  (define (struct-syntax $name $fields)
    (lets
      ($size (length $fields))
      #`(begin
        #,(struct-constructor-syntax $name $fields)
        #,@(map (partial struct-accessor-syntax $name $fields) (indices $size))
        #,(struct->datum-syntax $name $fields))))

  ; --------------------------------------

  (define (one-of-constructor-syntax $name $cases $index)
    (lets
      ($case (list-ref $cases $index))
      ($constructor 
        (datum->syntax $name 
          (string->symbol 
            (string-append 
              (symbol->string (syntax->datum $case))
              "-"
              (symbol->string (syntax->datum $name))))))
      #`(define-syntax-rule (#,$constructor one-of)
        (cons #,$index one-of))))

  (define (one-of-switch-syntax $name $cases)
    (lets
      ($name-string (symbol->string (syntax->datum $name)))
      ($switch (build-identifier ($string $name) (string-append $string "-switch")))
      ($one-of (generate-temporary #`one-of))
      ($index (generate-temporary #`index))
      ($value (generate-temporary #`value))
      ($case-pred (lambda ($case) (build-identifier ($string $case) (string-append $string "?"))))
      ($tmps (map generate-temporary $cases))
      ($index-tmp (lambda ($index) (list-ref $tmps $index)))
      ($case-body (lambda ($case) (build-identifier ($string $case) (string-append $string "-body"))))
      ($case-pattern (lambda ($index $case) #`(#,($case-pred $case) #,($index-tmp $index))))
      ($case-rule (lambda ($index $case) #`(#,($case-pattern $index $case) #,($case-body $case))))
      #`(define-syntax #,$switch
        (syntax-rules (#,@(map $case-pred $cases))
          ((_ one-of #,@(map-indexed $case-rule $cases))
            (lets
              (#,$one-of one-of)
              (#,$index (car #,$one-of))
              (#,$value (cdr #,$one-of))
              (case #,$index
                #,@(map-indexed 
                  (lambda ($case-index $case)
                    #`((#,$case-index) 
                      (lets
                        (#,($index-tmp $case-index) #,$value)
                        #,($case-body $case))))
                  $cases))))))))
  
  (define (one-of->datum-syntax $name $cases)
    (lets
      ($name->datum (build-identifier ($string $name) (string-append $string "->datum")))
      ($switch (build-identifier ($string $name) (string-append $string "-switch")))
      ($tmp (generate-temporary #`one-of))
      ($case->datum (lambda ($case) (build-identifier ($string $case) (string-append $string "->datum"))))
      ($case-pred (lambda ($case) (build-identifier ($string $case) (string-append $string "?"))))
      ($tmps (map generate-temporary $cases))
      ($index-tmp (lambda ($index) (list-ref $tmps $index)))
      ($case-pattern (lambda ($index $case) #`(#,($case-pred $case) #,($index-tmp $index))))
      ($case-rule (lambda ($index $case) 
        #`(#,($case-pattern $index $case) 
        `(#,$name ,(#,($case->datum $case) #,($index-tmp $index))))))
      #`(define (#,$name->datum #,$tmp)
        (#,$switch #,$tmp 
          #,@(map-indexed $case-rule $cases)))))

  (define (one-of-syntax $name $cases)
    (lets
      ($size (length $cases))
      #`(begin
        #,@(map (partial one-of-constructor-syntax $name $cases) (indices $size))
        #,(one-of-switch-syntax $name $cases)
        #,(one-of->datum-syntax $name $cases))))

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
