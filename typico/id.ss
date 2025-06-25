(library (typico id)
  (export id? id->symbol syntax->selector-id?)
  (import (micascheme))

  (define (syntax->selector-id? $syntax)
    (syntax-case? $syntax ()
      (id
        (id? #'id)
        #'id)
      ((id . _)
        (id? #'id)
        #'id)))

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
