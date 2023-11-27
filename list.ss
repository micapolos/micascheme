(library (list)
  (export 
    list-get
    list-get-overflow list-get-overflow? list-get-overflow-index

    opt
    single? single force-single
    ordered-map
    bind-if
    opt-lets
    opt-lift
    indices
    fold-while
    find-index
    list-set list-ref-opt list-drop
    associ
    filter-map filter-opts
    map-find-indexed
    map-indexed list-indexed
    indexed-find
    intercalate
    null

    flatten)

  (import
    (scheme)
    (syntax)
    (binder)
    (boolean)
    (check)
    (identifier)
    (indexed)
    (procedure)
    (data)
    (lets)
    (throw)
    (switch)
    (stack)
    (generate)
    (number))

  (define (flatten $lists) (apply append $lists))

  (define (single $list)
    (and (single? $list) (car $list)))

  (define (single? $list)
    (and
      (pair? $list) 
      (null? (cdr $list))))

  (define (force-single $list)
    (car (ensure single? $list)))

  (define (ordered-map $fn $list)
    (switch $list
      ((null? _) (list))
      ((else $pair)
        (cons
          ($fn (car $pair))
          (ordered-map $fn (cdr $pair))))))

  (define (bind-if $pred $obj $fn)
    (if ($pred $obj) ($fn $obj) $obj))

  (define-syntax opt-lets
    (syntax-rules ()
      ((_ body) body)
      ((_ (val expr) decl ... body)
        (let ((val expr)) 
          (and val (opt-lets decl ... body))))))

  (define-aux-keyword opt)

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

  (define-syntax null
    (lambda ($syntax)
      (cond
        ((identifier? $syntax) (syntax '()))
        (else (syntax-error $syntax)))))

  ; --------------------------------------

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

  (data (list-get-overflow index))

  (define (list-get $list $index)
    (if (null? $list)
      (list-get-overflow $index)
      (if (= $index 0)
        (car $list)
        (list-get (cdr $list) (- $index 1)))))

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