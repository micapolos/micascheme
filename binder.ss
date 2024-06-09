(library (binder)
  (export
    define-binder  ; TODO: Replace all usages with define-bind and remove
    define-bind
    transform-binder
    transform-binder-params)
  (import
    (scheme)
    (boolean)
    (identifier)
    (syntax))

  (define-aux-keyword bind)

  ; TODO: Replace all usages with define-bind and remove
  (define-rule-syntax (define-binder $name $binder)
    (define-bind $name
      (syntax-rules ()
        ((_ ($name . $params) $body)
          ($binder $name (lambda $params $body))))))

  (define-rule-syntax (define-bind $name $bind)
    (define-property $name bind $bind))

  (define (transform-binder $lookup $pattern $expr $body)
    (syntax-case $pattern ()
      ($name
        (identifier? #'$name)
        #`(let (($name #,$expr)) #,$body))
      (($name . $params)
        (and (identifier? #'$id) ($lookup #'$name #'bind))
        #`(let (($id #,$expr))
          #,(
            ($lookup #'$name #'bind)
            #`(bind ($id . $params) #,$body))))))

  (define (binding $decl)
    (cond
      ((identifier? $decl)
        (cons $decl #f))
      (else
        (cons (car (generate-temporaries '(tmp))) $decl))))

  (define (tail-binding $decl)
    (cond
      ((syntax-null? $decl)
        (cons $decl #f))
      (else
        (binding $decl))))

  (define (transform-binder-params $lookup $params $body)
    (syntax-case $params ()
      (($decl ... . $tail-decl)
        (let*
          (
            ($bindings
              (map binding (syntax->list #'($decl ...))))
            ($tail-binding
              (tail-binding #'$tail-decl)))
            #`(lambda (#,@(map car $bindings) . #,(car $tail-binding))
              #,(fold-left
                (lambda ($body $binding)
                  (let
                    (
                      ($identifier (car $binding))
                      ($decl-opt (cdr $binding)))
                    (or
                      (and $decl-opt
                        (transform-binder
                          $lookup
                          $decl-opt
                          $identifier
                          $body))
                      $body)))
                $body
                (cons $tail-binding (reverse $bindings))))))))
)
