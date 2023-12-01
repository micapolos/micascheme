(library (binder)
  (export
    define-binder
    transform-binder
    transform-monad)
  (import
    (scheme)
    (boolean)
    (identifier)
    (syntax)
    (switch))

  (define-aux-keyword accessors)
  (define-aux-keyword tail-accessor)

  (define-syntax define-binder
    (syntax-rules ()
      ((_ ($name $accessor ...))
        (define-property $name accessors (list (syntax $accessor) ...)))
      ((_ ($name $accessor ... . $tail-accessor))
        (begin
          (define-property $name accessors (list (syntax $accessor) ...))
          (define-property $name tail-accessor (syntax $tail-accessor))))))

  (define (transform-binder $lookup $pattern $expr $body)
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
                  #`(#,$id (#,$accessor #,$tmp)))
                $ids
                $accessors))
              #,(fold-left
                (lambda ($body $spec)
                  (transform-binder
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
                    #`(#,$id (#,$accessor #,$tmp)))
                  $ids
                  $accessors)
                ($last-id (#,$tail-accessor #,$tmp)))
              #,(fold-left
                (lambda ($body $spec)
                  (transform-binder
                    $lookup
                    (car $spec)
                    (cdr $spec)
                    $body))
                $body
                (reverse $specs))))))))

  (define (transform-monad $monad $expr $var $body)
    (switch $monad
      ((identifier? $identifier)
        #`(
          #,(build-identifier ($string $monad) (string-append $string "-bind"))
          #,$expr
          (lambda (#,$var) #,$body)))
      ((else $other)
        (switch (syntax->datum $other)
          ((false? _)
            #`(let ((#,$var #,$expr)) #,$body))
          ((else _)
            (syntax-error $monad "not a monad"))))))
)
