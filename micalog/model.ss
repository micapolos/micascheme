(library (micalog model)
  (export
    expr-domain expr-term expr-size)
  (import (micascheme))

  (define (expr-domain $expr)
    (syntax-case $expr ()
      ((domain _ _) #'domain)))

  (define (expr-size $expr)
    (syntax-case $expr ()
      ((_ size _) (datum size))))

  (define (expr-term $expr)
    (syntax-case $expr ()
      ((_ _ term) #'term)))
)
