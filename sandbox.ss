(import (micascheme))

(define-syntax kind
  (lambda ($syntax)
    (syntax-case $syntax ()
      ((_ $id1 $id2)
        (cond
          ((free-identifier=? #'$id1 #'dupa)
            #`(quote (dupa $id1 $id2)))
          ((bound-identifier=? #'$id1 #'$id2)
            #`(quote (bound $id1 $id2)))
          ((free-identifier=? #'$id1 #'$id2)
            #`(quote (free $id1 $id2)))
          (else
            #`(quote (nie-dupa $id1 $id2))))))))

(displayln (kind + +))
(displayln (kind + -))
(displayln (kind dupa dupa))
(displayln (kind dupa kupa))
