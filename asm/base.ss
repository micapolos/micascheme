(library (asm base)
  (export db dw)
  (import (micascheme) (asm asm) (asm syntax))

  (define-asm-syntax (db item ...) ($asm)
    (fold-left
      (lambda ($asm $item)
        (switch (syntax->datum $item)
          ((string? $string)
            (fold-left asm+u8 $asm
              (map literal->syntax (bytevector->u8-list (string->utf8 $string)))))
          ((char? $char)
            (asm+u8 $asm (literal->syntax (char->integer $char))))
          ((else $other)
            (asm+u8 $asm $other))))
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
