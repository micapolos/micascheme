(library (seq)
  (export
    null-seq cons-seq item-seq
    seq-next
    list->seq seq->list seq
    seq-append
    seq-flat-map seq-map)

  (import (chezscheme) (base))

  (define null-seq (lambda () `()))

  (define (cons-seq $value $seq)
    (lambda () (cons $value $seq)))

  (define (seq-next $seq) ($seq))

  (define (item-seq $item)
    (lambda () (cons-seq $item null-seq)))

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
      ((else $pair) (seq-fold $fn ($fn (car $pair)) (cdr $pair)))))

  (define (seq->list $seq)
    (reverse
      (seq-fold
        (lambda ($list $item) (cons $item $list))
        (list)
        $seq)))

  (define-syntax-rule (seq item ...)
    (list->seq (syntax->list #`(item ...))))

  (define (seq-append $lhs $rhs)
    (lambda ()
      (switch (seq-next $lhs)
        ((null? _) (seq-next $rhs))
        ((else $pair)
          (lets
            (car $pair)
            (seq-append (cdr $pair) $rhs))))))

  (define (seq-flat-map $seq $fn)
    (lambda ()
      (switch (seq-next $seq)
        ((null? _) null-seq)
        ((pair? $pair)
          (seq-next (seq-append ($fn (car $pair)) (seq-flat-map (cdr $pair) $fn)))))))

  (define (seq-map $seq $fn)
    (seq-flat-map $seq
      (lambda ($item)
        (item-seq $item))))
)
