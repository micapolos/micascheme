(library (micalog domain)
  (export
    edge+
    domain+
    env+id-domain)
  (import (micascheme) (prefix (micalog keywords) %))

  (define (edge+ $edge-a $edge-b)
    (if (syntax=? $edge-a $edge-b)
      $edge-a
      (syntax-error $edge-b "illegal edge")))

  (define (domain+ $domain-a $domain-b)
    (syntax-case $domain-a ()
      (() $domain-b)
      (((id-a . edge-a) . tail-a)
        (syntax-case $domain-b ()
          (() $domain-a)
          (((id-b . edge-b) . tail-b)
            (if (free-identifier=? (identifier id-a) (identifier id-b))
              #`(
                (id-a . #,(edge+ #'edge-a #'edge-b)) .
                #,(domain+ #'tail-a #'tail-b))
              (syntax-error #'id-b
                (format "illegal domain, expected ~a in" (datum id-a)))))))))

  (define (env+id-domain $env $id $domain)
    (syntax-update $env $id
      (lambda ($current-domain?)
        (if $current-domain?
          (domain+ $current-domain? $domain)
          $domain))))
)
