(library (syntax-match)
  (export define-syntax-match? syntax-match?)
  (import (scheme))

  (define-syntax (define-syntax-match? $syntax)
    (syntax-case $syntax ()
      ((_ id proc)
        #`(begin
          (define-syntax (id $syntax)
            (syntax-error $syntax "can not use matcher"))
          (define-property id syntax-match? proc)))))

  (define-syntax (syntax-match? $syntax)
    (lambda ($lookup)
      (define (transform $pattern $body)
        (syntax-case $pattern (syntax)
          ((syntax x)
            (values (list) #'x (list) $body))
          ((id . args)
            (let
              (($matcher? (and (identifier? #'id) ($lookup #'id #'syntax-match?))))
              (cond
                ($matcher? ($matcher? $pattern $body))
                (else
                  (let*-values
                    (
                      (($id-literals $id-pattern $id-fenders $id-body) (transform #'id $body))
                      (($args-literals $args-pattern $args-fenders $args-body) (transform #'args $id-body)))
                    (values
                      (append $id-literals $args-literals)
                      #`(#,$id-pattern . #,$args-pattern)
                      (append $id-fenders $args-fenders)
                      $args-body))))))
          (id
            (let
              (($matcher? (and (identifier? #'id) ($lookup #'id #'syntax-match?))))
              (cond
                ($matcher? ($matcher? $pattern $body))
                ((identifier? #'id) (values (list #'id) #'id (list) $body))
                (else (values (list) #'id (list) $body)))))))

      (syntax-case $syntax ()
        ((_ expr pattern fender body)
          (let-values
            ((($literals $pattern $fenders $body) (transform #'pattern #'body)))
            #`(syntax-case expr (#,@$literals)
              (#,$pattern (and #,@$fenders fender) #,$body)
              (_ #f))))
        ((_ expr pattern body)
          #`(syntax-match? expr pattern #t body)))))
)
