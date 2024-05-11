(import (micascheme) (string) (rust string))

(check (equal? (rust-expr-string #`(u8 10)) "10"))
(check (equal? (rust-expr-string #`(u8+1 (u8 10))) "10.wrapping_inc()"))
(check (equal? (rust-expr-string #`(u8-1 (u8 10))) "10.wrapping_dec()"))
(check (equal? (rust-expr-string #`(u8+ (u8 10) (u8 20))) "10.wrapping_add(20)"))
(check (equal? (rust-expr-string #`(u8- (u8 10) (u8 20))) "10.wrapping_sub(20)"))

(check (equal? (rust-body-string #`()) ""))

(check
  (equal?
    (rust-body-string #`((let (u8 10))))
    (lines-string "let v0 = 10;")))

(check
  (equal?
    (rust-body-string
      #`(
        (let (u8 10))
        (let (u8 20))))
    (lines-string
      "let v0 = 10;"
      "let v1 = 20;")))
