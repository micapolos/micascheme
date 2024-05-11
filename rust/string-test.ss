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
    (rust-block-string #`((val (rust "a"))))
    (lines-string "let v0 = a;")))

(check
  (equal?
    (rust-block-string
      #`(
        (val (rust "a"))
        (val (rust "b"))))
    (lines-string
      "let v0 = a;"
      "let v1 = b;")))

; in / get
(check
  (equal?
    (rust-block-string
      #`(
        (val (rust "a"))
        (val (rust "b"))
        (in (val (get 0)))))
    (lines-string
      "let v0 = a;"
      "let v1 = b;"
      "let v2 = v1;")))

(check
  (equal?
    (rust-block-string
      #`(
        (val (rust "a"))
        (val (rust "b"))
        (in
          (val (get 1))
          (val (get 0)))))
    (lines-string
      "let v0 = a;"
      "let v1 = b;"
      "let v2 = v0;"
      "let v3 = v1;")))

(check
  (equal?
    (rust-block-string
      #`(
        (val (rust "a"))
        (val (rust "b"))
        (in (val (get 1)))
        (in (val (get 0)))))
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
        (val (rust "a"))
        (val (rust "b"))
        (then
          (val (get 0))
          (val (get 1)))))
    (lines-string
      "let v0 = a;"
      "let v1 = b;"
      "let v2 = v1;"
      "let v3 = v0;")))

(check
  (equal?
    (rust-block-string
      #`(
        (val (rust "a"))
        (val (rust "b"))
        (then
          (val (get 0))
          (val (get 1)))
        (then
          (val (get 0))
          (val (get 1))
          (val (get 2))
          (val (get 3)))))
    (lines-string
      "let v0 = a;"
      "let v1 = b;"
      "let v2 = v1;"
      "let v3 = v0;"
      "let v4 = v3;"
      "let v5 = v2;"
      "let v6 = v1;"
      "let v7 = v0;")))
