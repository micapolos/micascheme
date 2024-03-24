(library (function)
  (export function)
  (import
    (scheme)
    (generate)
    (binder)
    (lets)
    (syntax)
    (switch))

  (define-lookup-syntax (function $syntax $lookup)
    (syntax-case $syntax ()
      ((_ $name $body)
        (identifier? #'$name)
        #`(define $name $body))
      ((_ ($name . $params) $body)
        (identifier? #'$name)
        #`(define $name
          #,(transform-binder-params
            $lookup
            #'$params
            #'$body)))))
)
