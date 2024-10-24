(import (micascheme) (code) (micac ast) (micac c-code))

(check (equal? (code-string (type->c-code (type (u8)))) "uint8_t"))
(check (equal? (code-string (type->c-code (type (u16)))) "uint16_t"))
(check (equal? (code-string (type->c-code (type (u32)))) "uint32_t"))

(check (equal? (code-string (const->c-code (const 128))) "128"))
(check (equal? (code-string (variable->c-code (variable 128))) "_128"))
(check (equal? (code-string (value->c-code (value (variable 128)))) "_128"))

(check
  (equal?
    (code-string (instr->c-code (instr (alloc (type (u8))))))
    (lines-string "uint8_t _0;")))

(check
  (equal?
    (code-string (instr->c-code (instr (ld (type (u8)) (variable 3) (value (const 128))))))
    (lines-string "_3 = 128;")))

(check
  (equal?
    (code-string (instr->c-code (instr (add (type (u8)) (variable 3) (value (const 128))))))
    (lines-string "_3 += 128;")))

(check
  (equal?
    (code-string
      (instr->c-code
        (instr
          (block
            (list
              (instr (alloc (type (u8))))
              (instr (alloc (type (u8))))
              (instr (ld (type (u8)) (variable 0) (value (const 128))))
              (instr (add (type (u8)) (variable 1) (value (variable 0)))))))))
    (lines-string
      "uint8_t _0;"
      "uint8_t _1;"
      "_0 = 128;"
      "_1 += _0;")))
