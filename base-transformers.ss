(library (base-transformers)
  (export
    binders
    transform-binders)
  (import (scheme))

  (define-syntax accessors
    (lambda (stx)
      (syntax-error stx "misplaced aux keyword")))

  (define-syntax tail-accessor
    (lambda (stx)
      (syntax-error stx "misplaced aux keyword")))

  (define-syntax binders
    (lambda ($syntax)
      (syntax-case $syntax ()
        ((_ ($name $accessor ...))
          #`(define-property $name accessors (quote ($accessor ...))))
        ((_ ($name $accessor ... . $tail-accessor))
          #`(begin
            (define-property $name accessors (quote ($accessor ...)))
            (define-property $name tail-accessor (quote $tail-accessor)))))))

  (define (transform-binders $lookup $pattern $expr $body)
    (syntax-case $pattern ()
      (($name $spec ...)
        (identifier? #'$name)
        (let*
          (
            ($accessors ($lookup #'$name #'accessors))
            ($tmp (car (generate-temporaries '(tmp))))
            ($id-spec-pairs
              (map
                (lambda ($spec)
                  (cond
                    ((identifier? $spec) (cons $spec #f))
                    (else
                      (let
                        (($tmp (car (generate-temporaries '(tmp)))))
                        (cons $tmp (cons $spec $tmp))))))
                (syntax->list #'($spec ...))))
            ($ids (map car $id-spec-pairs))
            ($specs (filter (lambda (t) t) (map cdr $id-spec-pairs))))
          (unless $accessors
            (syntax-error #'$name "not data"))
          (unless (= (length $accessors) (length $ids))
            (syntax-error #'($name $spec ...) "illegal number of matchers"))
          #`(let ((#,$tmp #,$expr))
            (let
              (#,@(map
                (lambda ($id $accessor)
                  #`(
                    #,$id
                    (
                      #,(datum->syntax #'$name $accessor)
                      #,$tmp)))
                $ids
                $accessors))
              #,(fold-left
                (lambda ($body $spec)
                  (transform-binders
                    $lookup
                    (car $spec)
                    (cdr $spec)
                    $body))
                $body
                (reverse $specs))))))
      (($name $spec ... . $last-id)
        (identifier? #'$name)
        (let*
          (
            ($accessors ($lookup #'$name #'accessors))
            ($tail-accessor ($lookup #'$name #'tail-accessor))
            ($tmp (car (generate-temporaries '(tmp))))
            ($id-spec-pairs
              (map
                (lambda ($spec)
                  (cond
                    ((identifier? $spec) (cons $spec #f))
                    (else
                      (let
                        (($tmp (car (generate-temporaries '(tmp)))))
                        (cons $tmp (cons $spec $tmp))))))
                (syntax->list #'($spec ...))))
            ($ids (map car $id-spec-pairs))
            ($specs (filter (lambda (t) t) (map cdr $id-spec-pairs))))
          (unless $accessors
            (syntax-error #'$name "not data"))
          (unless $tail-accessor
            (syntax-error #'$name "data without tail"))
          (unless (= (length $accessors) (length $ids))
            (syntax-error #'($name $spec ...) "illegal number of matchers"))
          #`(let ((#,$tmp #,$expr))
            (let
              (
                #,@(map
                  (lambda ($id $accessor)
                    #`(
                      #,$id
                      (
                        #,(datum->syntax #'$name $accessor)
                        #,$tmp)))
                  $ids
                  $accessors)
                (
                  $last-id
                  (
                    #,(datum->syntax #'$name $tail-accessor)
                    #,$tmp)))
              #,(fold-left
                (lambda ($body $spec)
                  (transform-binders
                    $lookup
                    (car $spec)
                    (cdr $spec)
                    $body))
                $body
                (reverse $specs))))))))
)
