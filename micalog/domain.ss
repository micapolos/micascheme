(library (micalog domain)
  (export domain+)
  (import
    (micascheme)
    (micalog utils)
    (prefix (micalog keywords) %))

  (define (domain+ $domain-a $domain-b)
    (syntax-case $domain-a ()
      (() #'())
      (((id-a edge-a) . tail-a)
        (syntax-case $domain-b ()
          (() #'())
          (((id-b edge-b) . tail-b)
            (if (free-identifier=? (identifier id-a) (identifier id-b))
              (lets
                ($edge? (edge+? #'edge-a #'edge-b))
                (if $edge?
                  #`(
                    (id-a #,$edge?) .
                    #,(domain+ #'tail-a #'tail-b))
                  #'()))
              #'()))))))
)
