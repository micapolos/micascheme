(library (micalog domain)
  (export
    edges+edge
    edges+
    domain+)
  (import (micascheme) (prefix (micalog keywords) %))

  (define (edges+edge $edges $edge)
    (syntax-case $edges ()
      ((pos? neg?)
        (syntax-case $edge (%posedge %negedge)
          (%posedge #'(#t neg?))
          (%negedge #'(pos? #t))))))

  (define (edges+ $edge-a $edge-b)
    (syntax-case $edge-a ()
      ((pos-a? neg-a?)
        (syntax-case $edge-b ()
          ((pos-b? neg-b?)
            #`(
              #,(syntax-or #'pos-a? #'pos-b?)
              #,(syntax-or #'neg-a? #'neg-b?)))))))

  (define (domain+ $domain-a $domain-b)
    (syntax-case $domain-a ()
      (((id-a . edges-a) . tail-a)
        (syntax-case $domain-b ()
          (((id-b . edges-b) . tail-b)
            (if (free-identifier=? (identifier id-a) (identifier id-b))
              #`(
                (id-a . #,(edges+ #'edges-a #'edges-b)) .
                #,(domain+ #'tail-a #'tail-b))
              #`TODO))))))
)
