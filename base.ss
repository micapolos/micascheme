(library (base)
  (export 
    identity

    list-get
    list-get-overflow list-get-overflow? list-get-overflow-index

    failure failure? failure-value failure!
    fallible-bind fallible-let

    false?
    null-or-pair?
    opt
    from
    once-proc
    checking-once
    raises?
    app app-values
    (rename
      (slice list->slice)
      (slice! slice))
    slice? slice-items
    splice-value splice
    single? single force-single
    bindings-eval
    script
    ordered-map
    pi pi2
    bind-if
    opt-lets lets
    opt-lift
    nonnegative-integer?
    current-seconds
    works?
    check checking?
    generate-symbol generate-symbols
    with-generate-temporary-seed
    with-tmps
    ensure
    data enum
    partial
    define-aux-keyword define-syntax-rule define-syntax-case
    displayln writeln
    indices iterate
    fold-while
    find-index
    list-set list-ref-opt list-drop
    switch switch-opt switch-exclusive
    or-throw
    unpair pair-values
    associ
    filter-map filter-opts
    map-find-indexed
    map-indexed list-indexed
    indexed-find
    indexed indexed? indexed-value indexed-index
    intercalate
    throw
    identifier-named?
    todo TODO
    null

    stack push push-list push-all top pop
    flatten
    gen-stack gen-list

    generate-temporary
    build-identifier

    fract

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
  (define (push-list $stack $list) (fold-left push $stack $list))
  (define (push-all $stack $stack2) (append $stack2 $stack))
  (define (top $stack) (car $stack))
  (define (pop $stack) (cdr $stack))
  (define (flatten $lists) (apply append $lists))

  (define-syntax stack
    (lambda ($syntax)
      (syntax-case $syntax ()
        ((_ $item ...) 
          #`(list #,@(reverse (syntax->list #`($item ...))))))))

  (define (gen-stack $proc $size)
    (iterate
      (lambda ($stack)
        (push $stack ($proc)))
      (stack)
      $size))

  (define (gen-list $proc $size)
    (reverse (gen-stack $proc $size)))

  (define (single $list)
    (and (single? $list) (car $list)))

  (define (single? $list)
    (and
      (pair? $list) 
      (null? (cdr $list))))

  (define (force-single $list)
    (car (ensure single? $list)))

  (define (generate-symbol)
    (syntax->datum (generate-temporary)))

  (define (generate-symbols $count)
    (reverse
      (iterate
        (lambda ($stack)
          (push $stack (generate-symbol)))
        (stack)
        $count)))

  (define (ordered-map $fn $list)
    (switch $list
      ((null? _) (list))
      ((else $pair)
        (cons
          ($fn (car $pair))
          (ordered-map $fn (cdr $pair))))))

  (define generate-temporary
    (case-lambda
      (()
        (or
          (generate-seeded-temporary)
          (car (generate-temporaries `(tmp)))))
      (($obj)
        (or
          (generate-seeded-temporary)
          (if (checking?)
            (build-identifier ($string $obj) (string-append "$" $string))
            (car (generate-temporaries (list $obj))))))))

  (define generate-temporary-seed-opt
    (make-thread-parameter #f))

  (define (generate-seeded-temporary)
    (lets
      ($seed-opt (generate-temporary-seed-opt))
      (and $seed-opt
        (let ()
          (generate-temporary-seed-opt (cons (car $seed-opt) (+ (cdr $seed-opt) 1)))
          (datum->syntax #`+
            (string->symbol
              (string-append
                (symbol->string (car $seed-opt))
                "-"
                (number->string (cdr $seed-opt)))))))))

  (define-syntax define-syntax-rule
    (syntax-rules ()
      ((_ (name param ...) body)
        (define-syntax name
          (syntax-rules ()
            ((_ param ...) body))))))

  (define-syntax define-syntax-case
    (lambda ($syntax)
      (syntax-case $syntax ()
        ((_ ($name $param ...) $body)
          #`(define-syntax-case ($name $param ...) () $body))
        ((_ ($name $param ...) $keywords $body)
          #`(define-syntax-case $name $keywords
            ((_ $param ...) $body)))
        ((_ $name $keywords $case ...)
          (let (($tmp (car (generate-temporaries `(tmp)))))
            #`(define-syntax $name
              (lambda (#,$tmp)
                (syntax-case #,$tmp $keywords
                  $case ...))))))))

  (define-syntax-rule (with-generate-temporary-seed $prefix $body ...)
    (parameterize ((generate-temporary-seed-opt (cons (quote $prefix) 0)))
      $body ...))

  (define-syntax-rule (with-tmps $body ...)
    (with-generate-temporary-seed $tmp $body ...))

  (define-syntax-rule (build-identifier ($var $id) $body)
    (datum->syntax $id
      (string->symbol 
        (lets ($var (symbol->string (syntax->datum $id))) $body))))

  (define (bind-if $pred $obj $fn)
    (if ($pred $obj) ($fn $obj) $obj))

  (define (null-or-pair? $obj)
    (or (null? $obj) (pair? $obj)))

  (define-syntax-rule (once-proc $proc)
    (let ()
      (define $applied? #f)
      (lambda ()
        (when $applied? (throw once-proc $proc))
        (set! $applied? #t)
        ($proc))))

  (define-syntax-rule (checking-once $body)
    (let ()
      (define $applied? #f)
      (lambda ()
        (when $applied? (error `checking-once "called twice" (quote $body)))
        (set! $applied? #t)
        $body)))

  (define (raises? $proc)
    (call/cc
      (lambda (cont)
        (with-exception-handler
          (lambda (_) (cont #t))
          (lambda () ($proc) #f)))))

  (define-syntax lets
    (lambda ($syntax)
      (syntax-case $syntax (do)
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
        ((_ (do $result)) #`$result)
        ((_ $result) #`$result))))

  (define-syntax opt-lets
    (syntax-rules ()
      ((_ body) body)
      ((_ (val expr) decl ... body)
        (let ((val expr)) 
          (and val (opt-lets decl ... body))))))

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

  (define-syntax-rule (app $fn $arg ...)
    ($fn $arg ...))

  (define (nonnegative-integer? $obj)
    (and (integer? $obj) (nonnegative? $obj)))

  (define-syntax app-values
    (lambda ($syntax)
      (syntax-case $syntax ()
        ((_ $fn $item ...)
          (lets
            ($arity-expr-pairs
              (map
                (lambda ($item)
                  (syntax-case $item ()
                    (($number $expr)
                      (lets
                        ($datum (syntax->datum #'$number))
                        (and (integer? $datum) (nonnegative? $datum)))
                      (cons (syntax->datum #'$number) #'$expr))
                    ($expr
                      (cons 1 #'$expr))))
                (syntax->list #'($item ...))))
            ($arities (map car $arity-expr-pairs))
            ($exprs (map cdr $arity-expr-pairs))
            (cond
              ((for-all (lambda ($arity) (= $arity 1)) $arities)
                #`($fn #,@$exprs))
              (else
                (lets
                  ($tmps (map generate-temporaries (map iota $arities)))
                  #`(let-values
                      (
                        #,@(map
                          (lambda ($tmps $expr) #`((#,@$tmps) #,$expr))
                          $tmps $exprs))
                      ($fn #,@(apply append $tmps)))))))))))

  (define-syntax-rule (define-aux-keyword aux)
    (define-syntax aux
      (lambda (stx)
        (syntax-error stx "misplaced aux keyword"))))

  (define-aux-keyword opt)

  (define (false? $value)
    (not $value))

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

  (define-syntax-rule (switch-opt $expr (($pred $var) $body ...) ...)
    (switch $expr
      (($pred $var) $body ...) ...
      ((else _) #f)))

  (define-syntax-rule (switch-exclusive $expr (($pred $var) $body ...) ...)
    (switch $expr
      (($pred $var) $body ...) ...
      ((else _)
        (throw non-exclusive
          (quote (switch $expr $pred ...))))))

  (define (fold-while $pred $fn $initial $list)
    (cond
      ((or (null? $list) (not ($pred $initial))) $initial)
      (else (fold-while $pred $fn ($fn $initial (car $list)) (cdr $list)))))

  (define indices iota)

  (define (map-indexed $proc $list)
    (map $proc (indices (length $list)) $list))

  (define (filter-map $proc . $list)
    (filter (lambda (x) x) (apply map (cons $proc $list))))

  (define (filter-opts $opts)
    (filter identity $opts))

  (define-syntax opt-lift
    (lambda ($syntax)
      (syntax-case $syntax ()
        ((_ $fn $spec ...)
          (lets
            ($opt-arg-pairs
              (map
                (lambda ($spec)
                  (syntax-case $spec (opt)
                    ((opt $arg) (cons #t #`$arg))
                    ($arg (cons #f #`$arg))))
                (syntax->list #`($spec ...))))
            ($opts (map car $opt-arg-pairs))
            ($args (map cdr $opt-arg-pairs))
            ($tmps (generate-temporaries $args))
            ($opt-tmps
              (filter (lambda (x) x)
                (map
                  (lambda ($opt $tmp) (and (not $opt) $tmp))
                  $opts $tmps)))
            #`(lets
              #,@(map (lambda ($tmp $arg) #`(#,$tmp #,$arg)) $tmps $args)
              (and #,@$opt-tmps ($fn #,@$tmps))))))))

  (define (list-indexed $list)
    (map-indexed (lambda ($index $value) (indexed $value $index)) $list))

  (define-syntax data
    (lambda (stx)
      (syntax-case stx ()
        ((_ (name field ...))
          (lets
            (name-string (symbol->string (syntax->datum #`name)))
            (tmp (car (generate-temporaries '(tmp))))
            (record-name (build-identifier ($string #`name) (string-append "%" $string)))
            (rtd-name (build-identifier ($string #`name) (string-append $string "-rtd")))
            (prefix-name (string-append name-string "-"))
            (predicate-name (build-identifier ($string #`name) (string-append $string "?")))
            #`(begin
              (define #,rtd-name
                (let ((#,tmp
                  (make-record-type #,name-string
                    (list '(immutable field) ...))))
                  (record-writer #,tmp
                    (record-pretty-writer #,tmp #,name-string))
                  (record-type-equal-procedure
                    #,tmp
                    (lambda (a b eq)
                      (and
                      #,@(map
                        (lambda (field)
                          (lets
                            (fld (build-identifier (s field) (string-append name-string "-" s)))
                            #`(eq (#,fld a) (#,fld b))))
                        (syntax->list #`(field ...))))))
                  (record-type-hash-procedure
                    #,tmp
                    (lambda (a hash)
                      (+
                      #,@(map
                        (lambda (field)
                          (lets
                            (fld (build-identifier (s field) (string-append name-string "-" s)))
                            #`(hash (#,fld a))))
                        (syntax->list #`(field ...))))))
                  #,tmp))
              (define name
                (record-constructor #,rtd-name))
              (define #,predicate-name
                (record-predicate #,rtd-name))
              #,@(map
                (lambda (index f)
                  #`(define #,(build-identifier (s f) (string-append prefix-name s))
                    (record-accessor #,rtd-name #,index)))
                (iota (length (syntax->list #'(field ...))))
                (syntax->list #'(field ...)))))))))

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

  (define pi (* (asin 1) 2))
  (define pi2 (* (asin 1) 4))

  (define (current-seconds)
    (lets
      ($time (current-time `time-monotonic))
      (+
        (time-second $time)
        (/ (time-nanosecond $time) 1000000000.0))))

  (define-syntax unpair
    (lambda (stx)
      (syntax-case stx ()
        ((_ expr lhs rhs body ...)
          (let ((tmp (car (generate-temporaries `(tmp)))))
            #`(let ((#,tmp expr))
              (let ((lhs (car #,tmp))
                    (rhs (cdr #,tmp)))
                body ...)))))))

  (define (pair-values $pair)
    (values (car $pair) (cdr $pair)))

  (define-syntax throw
    (lambda (stx)
      (syntax-case stx ()
        ((_ name item ...) (identifier? #`name)
          #`(error #f (format "~s" (list (quote name) #,@(syntax->list #`(item ...)))))))))

  (define-syntax or-throw
    (lambda ($syntax)
      (syntax-case $syntax ()
        ((_ ($target $arg ...))
          (identifier? #'$target)
          #'(or
            ($target $arg ...)
            (throw or-throw ($target $arg ...))))
        ((_ $other)
          #'(or $other
            (throw or-throw $other))))))

  (define (list-ref-opt $list $index)
    (and
      (pair? $list)
      (if (= $index 0)
        (car $list)
        (list-ref-opt (cdr $list) (- $index 1)))))

  (define (list-drop $list $count)
    (cond
      ((= $count 0) $list)
      (else (and (pair? $list) (list-drop (cdr $list) (- $count 1))))))

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

  (meta define (syntax->location-string $syntax)
    (lets
      ($annotation (syntax->annotation $syntax))
      (or
        (and $annotation
          (lets
            ($source (annotation-source $annotation))
            (($path $line $column) (locate-source-object-source $source #t #t))
            (format " source: ~a (~a:~a)\n" $path $line $column)))
        "")))

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
            (ann-string (syntax->location-string stx))
            #`(parameterize ((checking? #t))
              (let (#,@let-cases)
                (or
                  (not (pred #,@tmps))
                  (error `check
                    (format "\n~a   expr: ~s\n  value: ~s\n"
                      #,ann-string
                      (quote (not (pred arg ...)))
                      (list (quote not) (list (quote pred) #,@tmps)))))))))
        ((_ (pred arg ...))
          (lets
            (args (syntax->list #`(arg ...)))
            (tmps (generate-temporaries #`(arg ...)))
            (let-cases (map (lambda (tmp arg) #`(#,tmp #,arg)) tmps args))
            (ann (syntax->annotation stx))
            (source (annotation-source ann))
            (ann-string (syntax->location-string stx))
            #`(parameterize ((checking? #t))
              (let (#,@let-cases)
                (or
                  (pred #,@tmps)
                  (error `check
                    (format "\n~a   expr: ~s\n  value: ~s\n"
                      #,ann-string
                      (quote (pred arg ...))
                      (list (quote pred) #,@tmps)))))))))))

  (define-syntax ensure
    (lambda ($syntax)
      (syntax-case $syntax ()
        ((_ $pred $expr) (identifier? #`$pred)
          (lets
            ($tmp (car (generate-temporaries `(tmp))))
            #`(lets
              (#,$tmp $expr)
              (if ($pred #,$tmp)
                #,$tmp
                (throw ensure (quote $pred) #,$tmp))))))))

  (define-syntax identifier-named?
    (lambda ($syntax)
      (syntax-case $syntax ()
        ((_ $syntax $name) (identifier? #`$name)
          #`(and
            (identifier? $syntax)
            (symbol=? (syntax->datum $syntax) (quote $name)))))))

  (define (todo)
    (throw todo))

  (define-syntax TODO
    (lambda ($syntax)
      (cond
        ((identifier? $syntax) (syntax (todo)))
        (else (syntax-error $syntax)))))

  (define-syntax null
    (lambda ($syntax)
      (cond
        ((identifier? $syntax) (syntax '()))
        (else (syntax-error $syntax)))))

  ; --------------------------------------

  (data (failure value))

  (define-syntax-rule (failure! value)
    (failure (quote value)))

  (define (fallible-bind $fallible $fn)
    (switch $fallible
      ((failure? $failure) $failure)
      ((else $success) ($fn $success))))

  (define-syntax-rule (fallible-let ($success $fallible) $body ...)
    (fallible-bind $fallible
      (lambda ($success)
        $body ...)))

  ; --------------------------------------

  (data (slice items))

  (define (slice! . $items)
    (case (length $items)
      ((1) (car $items))
      (else (slice $items))))

  (define (splice-value $value)
    (switch $value
      ((slice? $slice) (slice-items $slice))
      ((else $value) (list $value))))

  (define (splice . $values)
    (apply append (map splice-value $values)))

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
    (opt-lets ($indexed (map-find-indexed $proc $list))
      (indexed-index $indexed)))

  (define (indexed-find+ $proc $list $index)
    (and
      (not (null? $list))
      (or
        ($proc $index (car $list))
        (indexed-find+ $proc (cdr $list) (+ $index 1)))))

  (define (indexed-find $proc $list)
    (indexed-find+ $proc $list 0))

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

  (define bindings-parameter
    (make-thread-parameter (stack)))

  (define evaluate-environment
    (lets
      ($environment (copy-environment (scheme-environment)))
      (do (define-top-level-value `bindings-parameter bindings-parameter $environment))
      $environment))

  (define (bindings-eval $bindings $datum)
    (lets
      ($datum
        `(let-values
          ((
            (,@(map car $bindings))
            (apply values (map cdr (bindings-parameter)))))
          ,$datum))
      (parameterize ((bindings-parameter $bindings))
        (eval $datum evaluate-environment))))

  (define (push-intercalated $stack $item $list)
    (cond
      ((null? $list) $stack)
      (else
        (push-intercalated
          (push
            (if (null? $stack) $stack (push $stack $item))
            (car $list))
          $item
          (cdr $list)))))

  (define (intercalate $list $item)
    (reverse (push-intercalated (stack) $item $list)))

  ; --------------------------------------

  (define (fract $number)
    (- $number (floor $number)))

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

  (assert (= (unpair (cons 3 2) l r (- l r)) 1))
)
