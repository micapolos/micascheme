(library (list)
  (export 
    bindable-list
    list-get
    list-get-overflow list-get-overflow? list-get-overflow-index

    opt
    opt->list
    non-false-list
    single? single force-single
    ordered-map
    map-using
    map-find
    filter-using
    find-using
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
    build-list
    ensure-list

    acc-split
    split

    flatten
    values->list

    assp-update
    assp-update-new

    assoc-update
    assoc-update-new

    assid
    assid-update
    assid-update-new

    group-by
    product
    map-product

    define-list->/append)

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
    (number)
    (pair))

  (define bindable-list list)

  (define-bind bindable-list
    (syntax-rules ()
      ((_ ($list . $values) $body)
        (apply (lambda $values $body) $list))))

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

  (define (acc-split $acc $list $n)
    (cond
      ((= $n 0) (values $acc $list))
      (else
        (lets
          ((pair $item $list) $list)
          (acc-split (cons $item $acc) $list (sub1 $n))))))

  (define (split $list $n)
    (lets
      ((values $acc $list) (acc-split (list) $list $n))
      (values (reverse $acc) $list)))

  (define indices iota)

  (define (map-indexed $proc $list)
    (map $proc (indices (length $list)) $list))

  (define (filter-map $proc . $list)
    (filter (lambda (x) x) (apply map (cons $proc $list))))

  (define (filter-opts $opts)
    (filter identity $opts))

  (define-syntax (opt-lift $syntax)
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
            (and #,@$opt-tmps ($fn #,@$tmps)))))))

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

  (define-syntax (null $syntax)
    (cond
      ((identifier? $syntax) (syntax '()))
      (else (syntax-error $syntax))))

  ; --------------------------------------

  (define (map-find $proc $list)
    (switch $list
      ((null? _) #f)
      ((else $pair)
        (or
          ($proc (car $pair))
          (map-find $proc (cdr $pair))))))

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

  (define (build-list $size $proc)
    (reverse
      (iterate-indexed
        (lambda ($list $index)
          (cons ($proc $index) $list))
        (list)
        $size)))

  (define (ensure-list $obj)
    (switch $obj
      ((list? $list) $list)
      ((else $other) (list $other))))

  (define-rule-syntax (values->list $expr)
    (call-with-values (lambda () $expr) list))

  ; === ass lists ===

  (define (assp-update $pred $update $list)
    (cond
      ((null? $list) #f)
      (else
        (lets
          ((pair $entry $list) $list)
          ((pair $key $value) $entry)
          (cond
            (($pred $key)
              (pair (pair $key ($update $value)) $list))
            (else
              (opt-lets
                ($list (assp-update $pred $update $list))
                (pair $entry $list))))))))

  (define (assp-update-new $pred $update $new $list)
    (or
      (assp-update $pred $update $list)
      (pair
        (lets
          ((pair $key $value) ($new))
          (pair $key ($update $value)))
        $list)))

  (define (assoc-update $key $update $list)
    (assp-update (partial equal? $key) $update $list))

  (define (assoc-update-new $key $update $new $list)
    (assp-update-new
      (partial equal? $key)
      $update
      (lambda () (pair $key ($new)))
      $list))

  (define (assid $id $list)
    (assp (partial free-identifier=? $id) $list))

  (define (assid-update $id $update $list)
    (assp-update (partial free-identifier=? $id) $update $list))

  (define (assid-update-new $id $update $new $list)
    (assp-update-new
      (partial free-identifier=? $id)
      $update
      (lambda () (pair $id ($new)))
      $list))

  ; === group-by ===

  (define (group-by $key-proc $eq-proc $list)
    (reverse
      (map
        (lambda ($group)
          (lets
            ((pair $key $list) $group)
            (pair $key (reverse $list))))
        (fold-left
          (lambda ($groups $item)
            (lets
              ($key ($key-proc $item))
              (assp-update-new
                (partial $eq-proc $key)         ; pred
                (partial pair $item)            ; update
                (lambda () (pair $key (list)))  ; new
                $groups)))
          (list)
          $list))))

  (define (product $list . $lists)
    (flatten
      (map
        (lambda ($item)
          (map
            (partial cons $item)
            (switch $lists
              ((null? _) (list '()))
              ((else (pair $list $lists))
                (apply product $list $lists)))))
        $list)))

  (define (map-product $fn $list . $lists)
    (map
      (partial apply $fn)
      (apply product $list $lists)))

  (define (map-using $list $fn)
    (map $fn $list))

  (define (filter-using $list $fn)
    (filter $fn $list))

  (define (find-using $list $fn)
    (find $fn $list))

  (define (opt->list $opt)
    (if $opt (list $opt) (list)))

  (define (non-false-list . $item)
    (filter-opts $item))

  (define-case-syntax (define-list->/append (id list) body ...)
    (lets
      ($list->id (identifier-append #'define-list->/append #'list-> #'id))
      ($append-id (identifier-append #'define-list->/append #'id #'- #'append))
      #`(begin
        (define (#,$list->id list) body ...)
        (define (#,$append-id . list) (#,$list->id list)))))

)
