(library (micac)
  (export micac run externs macro)
  (import
    (scheme)
    (syntax)
    (list-syntax)
    (syntaxes)
    (micac keywords)
    (micac c)
    (micac run))
  (export (import (micac keywords)))
  (export (import (only (syntaxes) literals)))
  (export (import (only (scheme)
    include
    = < <= > >= + - * div
    begin if
    and or not
    ...
    bitwise-and bitwise-ior bitwise-xor bitwise-not
    bitwise-arithmetic-shift-left bitwise-arithmetic-shift-right
    when cond else)))
  (export (import (only (micascheme) check equal?)))

  (define-keywords run externs)

  (define-syntax micac
    (lambda ($syntax)
      (syntax-case $syntax ()
        ((_ item ...)
          #`(begin
            #,@(map
              (lambda ($item)
                (syntax-case $item (run externs macro)
                  ((run body ...)
                    #`(micac-run body ...))
                  ((externs body ...)
                    #`(micac-externs body ...))
                  ((macro body ...)
                    #`(micac-macro body ...))))
              (syntax->list #'(item ...))))))))
)
