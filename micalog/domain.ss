(library (micalog domain)
  (export
    empty-domain
    domain+
    env-domain+register)
  (import
    (micascheme)
    (micalog utils)
    (prefix (micalog keywords) %))

  (define (empty-domain) #'())

  (define domain=? syntax=?)

  (define (domain+ $domain-a $domain-b)
    (syntax-case $domain-a ()
      (() (empty-domain))
      (((id-a edge-a) . tail-a)
        (syntax-case $domain-b ()
          (() (empty-domain))
          (((id-b edge-b) . tail-b)
            (if (free-identifier=? (identifier id-a) (identifier id-b))
              (lets
                ($edge? (edge+? #'edge-a #'edge-b))
                (if $edge?
                  #`((id-a #,$edge?) . #,(domain+ #'tail-a #'tail-b))
                  (empty-domain)))
              (empty-domain)))))))

  (define (env+id-domain $env $id $domain)
    (syntax-set $env $id $domain))

  (define (env-id->domain $env $id)
    (syntax-ref $env $id))

  (define (env-domain+register $env $domain $register)
    (syntax-case $register (%register)
      ((%register type name)
        (syntax-set $env (identifier name) $domain))))

  (define (env-domain+set $env $domain $set)
    (syntax-case $set (%set)
      ((%set type name expr)
        (lets
          ($register-domain (env-id->domain $env (identifier name)))
          (if (domain=? $register-domain $domain)
            TODO
            (syntax-error #'name
              (format
                "illegal domain ~a, expected ~a in register"
                $register-domain
                $domain)))))))

  (define (env-id-domain-ref $env $id $domain)
    (lets
      ($env-domain (env-id->domain $env $id))
      (if (domain=? $env-domain $domain)
        $domain
        (syntax-error $id
          (format
            "illegal domain ~a, expected ~a in"
            $env-domain
            $domain)))))
)
