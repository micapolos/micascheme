(library (typed type)
  (export
    arrow arrow? arrow-params arrow-result
    type=?
    any-string
    define-type)
  (import (micascheme))

  (define-syntax (define-type $syntax)
    (syntax-case $syntax ()
      ((_ id)
        (identifier? #'id)
        #`(define-values
          (id #,(identifier-append #'id #'id #'?))
          (lets
            ($name #,(symbol->string (datum id)))
            ($rtd (make-record-type $name `()))
            (run
              (record-writer $rtd
                (lambda ($record $port $wr)
                  (display $name $port))))
            (values
              ((record-constructor $rtd))
              (record-predicate $rtd)))))))

  (data (arrow params result))

  (define (type=? $type-a $type-b)
    (switch $type-a
      ((arrow? $arrow-a)
        (switch? $type-b
          ((arrow? $arrow-b)
            (and
              (=
                (length (arrow-params $arrow-a))
                (length (arrow-params $arrow-b)))
              (for-all type=?
                (arrow-params $arrow-a)
                (arrow-params $arrow-b))
              (type=?
                (arrow-result $arrow-a)
                (arrow-result $arrow-b))))))
      ((identifier? $identifier-a)
        (switch? $type-b
          ((identifier? $identifier-b)
            (free-identifier=? $identifier-a $identifier-b))))
      ((else $other-a)
        (equal? $other-a $type-b))))

  (define-syntax (any-string $syntax)
    (syntax-case $syntax ()
      (x (identifier? #'x)
        #'#'any-string)))
)
