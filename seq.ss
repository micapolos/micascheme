(library (seq)
  (export
    null-seq cons-seq
    seq-next
    seq=?
    list->seq seq->list seq
    seq-append
    seq-flat-map seq-map
    indexed-seq-from indexed-seq)

  (import
    (scheme)
    (indexed)
    (list)
    (lets)
    (switch))

  (define null-seq (lambda () `()))

  (define (cons-seq $value $seq)
    (lambda () (cons $value $seq)))

  (define (seq-next $seq) ($seq))

  (define (seq=? $lhs $rhs)
    (lets
      ($lhs-next (seq-next $lhs))
      ($rhs-next (seq-next $rhs))
      (cond
        ((null? $lhs-next) (null? $rhs-next))
        (else
          (and
            (pair? $rhs-next)
            (equal? (car $lhs-next) (car $rhs-next))
            (seq=? (cdr $lhs-next) (cdr $rhs-next)))))))

  (define (list->seq $list)
    (lambda ()
      (cond
        ((null? $list) $list)
        (else
          (cons
            (car $list)
            (list->seq (cdr $list)))))))

  (define (seq-fold $fn $init $seq)
    (switch (seq-next $seq)
      ((null? _) $init)
      ((else $pair) (seq-fold $fn ($fn $init (car $pair)) (cdr $pair)))))

  (define (seq->list $seq)
    (reverse
      (seq-fold
        (lambda ($list $item) (cons $item $list))
        (list)
        $seq)))

  (define-syntax seq
    (syntax-rules ()
      ((_) null-seq)
      ((_ item) (cons-seq item null-seq))
      ((_ item ...) (list->seq (list item ...)))))

  (define (seq-append $lhs $rhs)
    (lambda ()
      (switch (seq-next $lhs)
        ((null? _) (seq-next $rhs))
        ((else $pair)
          (cons
            (car $pair)
            (seq-append (cdr $pair) $rhs))))))

  (define (seq-flat-map $fn $seq)
    (lambda ()
      (switch (seq-next $seq)
        ((null? $null) $null)
        ((else $pair)
          (seq-next
            (seq-append
              ($fn (car $pair))
              (seq-flat-map $fn (cdr $pair))))))))

  (define (seq-map $fn $seq)
    (seq-flat-map 
      (lambda ($item) (seq ($fn $item))) 
      $seq))

  (define (indexed-seq-from $seq $index)
    (lambda ()
      (switch (seq-next $seq)
        ((null? $null) $null)
        ((else $pair)
          (cons
            (indexed (car $pair) $index)
            (indexed-seq-from (cdr $pair) (+ $index 1)))))))

  (define (indexed-seq $seq)
    (indexed-seq-from $seq 0))
)
