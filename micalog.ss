(library (micalog)
  (export micalog)
  (import
    (micascheme)
    (prefix (micalog keywords) %))
  (export (import (micalog keywords)))
  (export (import (only (micascheme) ...)))

  (define-syntax (micalog $syntax $lookup)
    (syntax-case $syntax ()
      ((_ declaration ...)
        #`(begin
          #,@(fluent
            (syntaxes declaration ...)
            (map-using
              (lambda ($declaration)
                (syntax-case $declaration (%macro)
                  ((%macro (name param ...) body ...)
                    #`(define-syntax name
                      (make-compile-time-value
                        (lambda ($syntax)
                          (syntax-case $syntax ()
                            ((_ arg (... ...))
                              (syntax-subst
                                #'(param ...)
                                #'(arg (... ...))
                                #'(begin body ...))))))))))))))))
)
