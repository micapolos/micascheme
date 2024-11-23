(library (micalog domain)
  (export
    empty-domain
    domain+)
  (import
    (micascheme)
    (micalog utils)
    (prefix (micalog keywords) %))

  (define (empty-domain) #'())

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
)
