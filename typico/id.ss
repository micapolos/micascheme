(library (typico id)
  (export id? id->symbol id=? syntax->selector-id?)
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

  (define (id=? $id-a $id-b)
    (symbol=? (id->symbol $id-a) (id->symbol $id-b)))
)
