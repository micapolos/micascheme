(library (leoscheme)
  (export leo with doing splice get is does)
  (import (micascheme))

  (define-aux-keyword does)
  (define-aux-keyword get)
  (define-aux-keyword with)
  (define-aux-keyword doing)
  (define-aux-keyword splice)

  (meta data (acc defs args))

  (meta define (empty-acc)
    (acc (stack) (stack)))

  (meta define (acc+def $acc $def)
    (acc
      (push (acc-defs $acc) $def)
      (acc-args $acc)))

  (meta define (acc+arg $acc $arg)
    (acc
      (acc-defs $acc)
      (push (acc-args $acc) $arg)))

  (meta define (acc+args $acc $args)
    (fold-left acc+arg $acc $args))

  (meta define (acc-apply $acc $fn)
    (acc
      (stack ($fn (reverse (acc-args $acc))))
      (acc-defs $acc)))

  (meta define (acc-syntax $acc)
    (lets
      ($defs (reverse (acc-defs $acc)))
      ($args (reverse (acc-args $acc)))
      ($arg (or (single $args) (syntax-error #`(#,@$args) "not single")))
      (cond
        ((null? $defs) $arg)
        (else #`(let () #,@$defs #,$arg)))))

  (define-syntax leo
    (lambda ($syntax)
      (define (syntax-leo $syntax)
        (syntax-case $syntax ()
          (($item ...)
            (acc-syntax
              (fold-left
                acc+syntax
                (empty-acc)
                (syntax->list #`($item ...)))))
          ($other #`$other)))

      (define (syntax-leos $syntax)
        (syntax-case $syntax ()
          (($item ...)
            (reverse
              (fold-left
                leos+syntax
                (stack)
                (syntax->list #`($item ...)))))
          ($other
            (list #`$other))))

      (define (leos+syntax $leos $syntax)
        (syntax-case $syntax (with doing apply splice get do)
          ((get $id) (identifier? #`$id)
            (leos+syntax $leos #`($id)))
          ((do $id) (identifier? #`$id)
            (leos+syntax $leos #`($id)))
          ((doing $body ...)
            (list
              #`(lambda (#,@(reverse $leos))
                #,(syntax-leo #`($body ...)))))
          ((apply $body ...)
            (list
              #`(
                #,@(reverse $leos)
                #,@(syntax-leos #`($body ...)))))
          ((with $item ...)
            (push $leos
              (syntax-leo #`($item ...))))
          ((splice $item ...)
            (push-list $leos
              (map syntax-leo
                (syntax->list #`($item ...)))))
          (($id $item ...) (identifier? #`$id)
            (list
              #`($id
                #,@(reverse $leos)
                #,@(syntax-leos #`($item ...)))))
          ($other
            (push $leos #`$other))))

      (define (leos-leo $leos)
        (or
          (single (reverse $leos))
          (syntax-error #`(#,@(reverse $leos)) "not single")))

      (define (acc+syntax $acc $syntax)
        (syntax-case $syntax (is does)
          ((is $item ...)
            (acc
              (push
                (acc-defs $acc)
                #`(define
                  #,(leos-leo (acc-args $acc))
                  #,(syntax-leo #`($item ...))))
              (stack)))
          ((does $item ...)
            (acc
              (push
                (acc-defs $acc)
                #`(define
                  #,(leos-leo (acc-args $acc))
                  #,(syntax-leo #`($item ...))))
              (stack)))
          ($other
            (acc
              (acc-defs $acc)
              (leos+syntax (acc-args $acc) #`$other)))))

      (syntax-case $syntax ()
        ((_ $item ...)
          (syntax-leo #`($item ...))))))
)
