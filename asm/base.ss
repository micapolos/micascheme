(library (asm base)
  (export db)
  (import (micascheme) (asm) (asm syntax))

  (define-asm-syntax (db item ...) ($asm)
    (fold-left
      (lambda ($asm $item)
        (switch (syntax->datum $item)
          ((string? $string)
            (asm+syntax $asm
              #`(u8
                #,@(map literal->syntax (bytevector->u8-list (string->utf8 $string))))))
          ((char? $char)
            (asm+syntax $asm
              #`(u8 #,(literal->syntax (char->integer $char)))))
          ((else _)
            (asm+syntax $asm
              #`(u8 #,$item)))))
      $asm
      (syntaxes item ...)))
)
