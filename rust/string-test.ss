(import (micascheme) (string) (rust string))

(check (equal? (rust-expr-string #`(u8 10)) "10"))
(check (equal? (rust-expr-string #`(u8+1 (u8 10))) "10.wrapping_inc()"))
(check (equal? (rust-expr-string #`(u8-1 (u8 10))) "10.wrapping_dec()"))
(check (equal? (rust-expr-string #`(u8+ (u8 10) (u8 20))) "10.wrapping_add(20)"))
(check (equal? (rust-expr-string #`(u8- (u8 10) (u8 20))) "10.wrapping_sub(20)"))

(check (equal? (rust-block-string #`()) ""))

; let
(check
  (equal?
    (rust-block-string #`((let (u8 10))))
    (lines-string "let v0 = 10;")))

; in
(check
  (equal?
    (rust-block-string
      #`(
        (let (u8 10))
        (let (u8 20))))
    (lines-string
      "let v0 = 10;"
      "let v1 = 20;")))

; get
(check
  (equal?
    (rust-block-string
      #`(
        (let (u8 10))
        (let (u8 20))
        (in (let (get 0)))))
    (lines-string
      "let v0 = 10;"
      "let v1 = 20;"
      "let v2 = v1;")))

(check
  (equal?
    (rust-block-string
      #`(
        (let (u8 10))
        (let (u8 20))
        (in (let (get 1)))))
    (lines-string
      "let v0 = 10;"
      "let v1 = 20;"
      "let v2 = v0;")))
