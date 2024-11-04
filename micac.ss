(library (micac)
  (export micac run externs macro)
  (import
    (scheme)
    (syntax)
    (list-syntax)
    (syntaxes)
    (micac syntax)
    (micac c)
    (micac run))
  (export (import (micac syntax)))
  (export (import (only (syntaxes) literals)))
  (export (import (only (scheme)
    = < <= > >= + - * /
    begin if
    not
    ...
    and or bitwise-and bitwise-ior bitwise-xor
    bitwise-arithmetic-shift-left bitwise-arithmetic-shift-right)))

  (define-aux-keywords run externs macro)

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
