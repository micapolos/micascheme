(library (typico id)
  (export id? id->symbol)
  (import (micascheme))

  (define (id? $syntax)
    (syntax-case? $syntax ()
      (id
        (symbol? (datum id))
        #t)))

  (define (id->symbol $syntax)
    (syntax-case? $syntax ()
      (id
        (id? #'id)
        (datum id))))
)
