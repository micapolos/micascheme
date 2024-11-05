(import (micascheme) (micac syntax) (micac unit))

(define-rule-syntax (check-decl in out)
  (check (equal? (syntax->datum (transform-decl #'in)) 'out)))

(check-decl (x bit) (var uint8_t x))

(check-decl (x (* bit 1)) (var uint8_t x))
(check-decl (x (* bit 8)) (var uint8_t x))
(check-decl (x (* bit 9)) (var uint16_t x))
(check-decl (x (* bit 16)) (var uint16_t x))
(check-decl (x (* bit 17)) (var uint32_t x))
(check-decl (x (* bit 32)) (var uint32_t x))
(check-decl (x (* bit 33)) (var uint64_t x))
(check-decl (x (* bit 64)) (var uint64_t x))

(check-decl (x (* (* bit 8) 65536)) (var uint8_t (* x 65536)))
(check-decl (x (* (* (* bit 8) 4) 16384)) (var uint8_t (* (* x 4) 16384)))
