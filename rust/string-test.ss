(import (micascheme) (string) (rust string))

; direct rust code
(check (equal? (rust-expr-string #`(rust "a")) "a"))

; u8
(check (equal? (rust-expr-string #`(u8 10)) "(10 as u8)"))
(check (equal? (rust-expr-string #`(u8+1 (rust "a"))) "a.wrapping_inc()"))
(check (equal? (rust-expr-string #`(u8-1 (rust "a"))) "a.wrapping_dec()"))
(check (equal? (rust-expr-string #`(u8+ (rust "a") (rust "b"))) "a.wrapping_add(b)"))
(check (equal? (rust-expr-string #`(u8- (rust "a") (rust "b"))) "a.wrapping_sub(b)"))

; let
(check (equal? (rust-block-string #`()) ""))

(check
  (equal?
    (rust-block-string #`((let (rust "a"))))
    (lines-string "let v0 = a;")))

(check
  (equal?
    (rust-block-string
      #`(
        (let (rust "a"))
        (let (rust "b"))))
    (lines-string
      "let v0 = a;"
      "let v1 = b;")))

; in / get
(check
  (equal?
    (rust-block-string
      #`(
        (let (rust "a"))
        (let (rust "b"))
        (in (let (get 0)))))
    (lines-string
      "let v0 = a;"
      "let v1 = b;"
      "let v2 = v1;")))

(check
  (equal?
    (rust-block-string
      #`(
        (let (rust "a"))
        (let (rust "b"))
        (in
          (let (get 1))
          (let (get 0)))))
    (lines-string
      "let v0 = a;"
      "let v1 = b;"
      "let v2 = v0;"
      "let v3 = v1;")))

(check
  (equal?
    (rust-block-string
      #`(
        (let (rust "a"))
        (let (rust "b"))
        (in (let (get 1)))
        (in (let (get 0)))))
    (lines-string
      "let v0 = a;"
      "let v1 = b;"
      "let v2 = v0;"
      "let v3 = v2;")))

; then
(check
  (equal?
    (rust-block-string
      #`(
        (let (rust "a"))
        (let (rust "b"))
        (then
          (let (get 0))
          (let (get 1)))))
    (lines-string
      "let v0 = a;"
      "let v1 = b;"
      "let v2 = v1;"
      "let v3 = v0;")))

(check
  (equal?
    (rust-block-string
      #`(
        (let (rust "a"))
        (let (rust "b"))
        (then
          (let (get 0))
          (let (get 1)))
        (then
          (let (get 0))
          (let (get 1))
          (let (get 2))
          (let (get 3)))))
    (lines-string
      "let v0 = a;"
      "let v1 = b;"
      "let v2 = v1;"
      "let v3 = v0;"
      "let v4 = v3;"
      "let v5 = v2;"
      "let v6 = v1;"
      "let v7 = v0;")))
