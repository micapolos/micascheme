(library (minic lang)
  (export minic)
  (import
    (micascheme)
    (minic keyword)
    (minic syntax)
    (minic type)
    (prefix (emu math) emu-))
  (export (import (minic keyword)))

  (define-case-syntax (minic body)
    (parse
      (env
        ; syntax->expr
        (lambda ($syntax)
          (and (identifier? $syntax)
            (cond
              ((free-identifier=? $syntax #'u8+1)
                (expr
                  (function-type (list (int-type 8)) (int-type 8))
                  #'emu-u8+1))
              ((free-identifier=? $syntax #'u8+)
                (expr
                  (function-type (list (int-type 8) (int-type 8)) (int-type 8))
                  #'emu-u8+))
              ((free-identifier=? $syntax #'u16+1)
                (expr
                  (function-type (list (int-type 16)) (int-type 16))
                  #'emu-u16+1))
              ((free-identifier=? $syntax #'u16+)
                (expr
                  (function-type (list (int-type 16) (int-type 16)) (int-type 16))
                  #'emu-u16+))
              (else #f))))
        ; syntax-type->value
        (lambda ($syntax $type) #f))
      #'body))
)
