(import (micascheme) (micac unit-syntax))

; transform-type
(check (equal? (syntax->datum (transform-type #'bit)) `uint8_t))

(check (equal? (syntax->datum (transform-type #'(* bit 1))) `uint8_t))
(check (equal? (syntax->datum (transform-type #'(* bit 8))) `uint8_t))
(check (equal? (syntax->datum (transform-type #'(* bit 9))) `uint16_t))
(check (equal? (syntax->datum (transform-type #'(* bit 16))) `uint16_t))
(check (equal? (syntax->datum (transform-type #'(* bit 17))) `uint32_t))
(check (equal? (syntax->datum (transform-type #'(* bit 32))) `uint32_t))
(check (equal? (syntax->datum (transform-type #'(* bit 33))) `uint64_t))
(check (equal? (syntax->datum (transform-type #'(* bit 64))) `uint64_t))

(check (equal? (syntax->datum (transform-type #'(* (* bit 8) 16))) `(* uint8_t 65536)))
(check (equal? (syntax->datum (transform-type #'(* (* (* bit 8) 2) 16))) `(* (* uint8_t 4) 65536)))
