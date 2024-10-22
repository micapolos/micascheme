(import (micascheme) (code) (micac ast) (micac c-code))

(check
  (equal?
    (code-string (type->c-code (type (bool))))
    "bool"))

(check
  (equal?
    (code-string (type->c-code (type (u8))))
    "uint8_t"))

(check
  (equal?
    (code-string (type->c-code (type (u16))))
    "uint16_t"))

(check
  (equal?
    (code-string (type->c-code (type (u32))))
    "uint32_t"))

(check
  (equal?
    (code-string
      (type->c-code
        (type
          (struct
            (stack
              (type (u8))
              (type (u16)))))))
    (lines-string0
      "struct {"
      "  uint8_t _0;"
      "  uint16_t _1;"
      "}")))

(check
  (equal?
    (code-string
      (statement->c-code
        (statement
          (return
            (expr
              (type (u8))
              (term (const 128)))))))
    "return 128;"))

(check
  (equal?
    (code-string
      (statement->c-code
        (statement
          (block
            (stack (type (u8)) (type (u16)))
            (stack)))))
    (lines-string0
      "{"
      "  uint8_t _0;"
      "  uint16_t _1;"
      "}")))
