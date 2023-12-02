(library (binder)
  (export
    define-binder
    transform-binder)
  (import
    (scheme)
    (boolean)
    (identifier)
    (syntax)
    (switch))

  (define-aux-keyword binder)

  (define-syntax-rule (define-binder $name $binder)
    (define-property $name binder (syntax $binder)))

  (define (transform-binder $lookup $pattern $expr $body)
    (syntax-case $pattern ()
      ($name
        (identifier? #'$name)
        #`(let (($name #,$expr)) #,$body))
      (($name $id ... . $tail-id)
        (identifier? #'$name)
        (let (($binder ($lookup #'$name #'binder)))
          (or
            (and $binder
              #`(#,$binder #,$expr
                (lambda ($id ... . $tail-id) #,$body)))
            (syntax-error $pattern "no binder for"))))))
)
