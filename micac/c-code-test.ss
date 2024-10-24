(import (micascheme) (code) (micac ast) (micac c-code))

(check (equal? (code-string (type->c-code (type (u8)))) "uint8_t"))
(check (equal? (code-string (type->c-code (type (u16)))) "uint16_t"))
(check (equal? (code-string (type->c-code (type (u32)))) "uint32_t"))

(check (equal? (code-string (const->c-code (const 128))) "128"))
(check (equal? (code-string (variable->c-code (variable 0) 2)) "v1"))
(check (equal? (code-string (variable->c-code (variable 1) 2)) "v0"))
(check (equal? (code-string (value->c-code (value (const 128)) 2)) "128"))

(check
  (equal?
    (code-string
      (instr->c-code
        (instr
          (alloc
            (list
              (type (u8))
              (type (u8))
              (type (u16)))
            (list
              (instr (ld (type (u8)) (variable 1) (value (const 128))))
              (instr (add (type (u8)) (variable 2) (value (variable 1))))
              (instr (alloc (list (type (u32))) (list))))))))
    (lines-string
      "uint8_t v0;"
      "uint8_t v1;"
      "uint16_t v2;"
      "v1 = 128;"
      "v0 += v1;"
      "uint32_t v3;")))
