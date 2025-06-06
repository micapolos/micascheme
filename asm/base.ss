(library (asm base)
  (export db dw)
  (import (micascheme) (asm asm) (asm syntax))

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
          ((else $other)
            (asm+syntax $asm #`(u8 #,$other)))))
      $asm
      (syntaxes item ...)))

  (define-asm-syntax (dw item ...) ($asm)
    (fold-left
      (lambda ($asm $item)
        (fluent $asm
          (asm+put
            (lambda ($port)
              #`(put-u16 #,$port #,$item (endianness little))))
          (asm+org 2)))
      $asm
      (syntaxes item ...)))
)
