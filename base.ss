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
    app values-app
    single? single force-single
    bindings-eval
    script
    ordered-map
    bind-if
    opt-lets
    opt-lift
    nonnegative-integer?
    current-seconds
    works?
    generate-symbol generate-symbols
    with-generate-temporary-seed
    with-tmps
    ensure
    partial
    displayln writeln logging
    indices iterate
    fold-while
    find-index
    list-set list-ref-opt list-drop
    unpair pair-values
    associ
    filter-map filter-opts
    map-find-indexed
    map-indexed list-indexed
    indexed-find
    indexed indexed? indexed-value indexed-index
    intercalate
    todo TODO
    null

    stack push push-list push-all top pop
    flatten
    gen-stack gen-list

    generate-temporary
    build-identifier)

  (import
    (scheme)
    (syntax)
    (binder)
    (check)
    (identifier)
    (data)
    (lets)
    (throw)
    (switch))

  (define identity (lambda (x) x))

  (define (displayln x) (display x) (newline))
  (define (writeln x) (write x) (newline))

  (define-syntax logging
    (syntax-rules ()
      ((_ $value)
        (let ()
          (writeln $value)
          $value))
      ((_ $label $value)
        (let ()
          (display (symbol->string (quote $label)))
          (display ": ")
          (writeln $value)
          $value))))

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

  (define-syntax-rule (with-generate-temporary-seed $prefix $body ...)
    (parameterize ((generate-temporary-seed-opt (cons (quote $prefix) 0)))
      $body ...))

  (define-syntax-rule (with-tmps $body ...)
    (with-generate-temporary-seed $tmp $body ...))

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

  (define-syntax values-app
    (lambda ($syntax)
      (syntax-case $syntax ()
        ((_ ($arity $expr) ...)
          (and
            (for-all integer? (datum ($arity ...)))
            (for-all nonnegative? (datum ($arity ...))))
          (lets
            ($arities (map syntax->datum (syntax->list #'($arity ...))))
            ($exprs (syntax->list #'($expr ...)))
            ($tmps (map generate-temporaries (map iota $arities)))
            #`(let-values
                (
                  #,@(map
                    (lambda ($tmps $expr) #`((#,@$tmps) #,$expr))
                    $tmps $exprs))
                (#,@(apply append $tmps))))))))

  (define-aux-keyword opt)

  (define (false? $value)
    (not $value))

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
)
