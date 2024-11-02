(import (micascheme) (micac syntax) (micac syntax-c) (check))

(define-aux-keyword macro)

(define $lookup
  (lambda ($id $key)
    (and
      (free-identifier=? $id #'macro)
      (free-identifier=? $key #'micac)
      (lambda ($syntax)
        (syntax-case $syntax ()
          ((_ arg ...)
            #`(macroed arg ...)))))))

(check
  (equal?
    (syntax-c $lookup)
    (lines-string)))

(check
  (equal?
    (syntax-c $lookup #`(var u8 x))
    (lines-string "uint8_t x;")))

(check
  (equal?
    (syntax-c $lookup #`(var u16 y))
    (lines-string "uint16_t y;")))

(check
  (equal?
    (syntax-c $lookup #`(var u32 z))
    (lines-string "uint32_t z;")))

(check
  (equal?
    (syntax-c $lookup #`(var (* u32) x))
    (lines-string "uint32_t* x;")))

(check
  (equal?
    (syntax-c $lookup #`(var (* (* u32)) x))
    (lines-string "uint32_t** x;")))

(check
  (equal?
    (syntax-c $lookup #`(var (* u32 24) x))
    (lines-string "uint32_t[24] x;")))

(check
  (equal?
    (syntax-c $lookup #`(var (* (* u32 24) 32) x))
    (lines-string "uint32_t[24][32] x;")))

(check
  (equal?
    (syntax-c $lookup #`(begin))
    (lines-string
      "{"
      "}")))

(check
  (equal?
    (syntax-c $lookup
      #`(begin
        (var u8 x)
        (set x 10)))
    (lines-string
      "{"
      "  uint8_t x;"
      "  x = 10;"
      "}")))

(check
  (equal?
    (syntax-c $lookup
      #`(var u8 x)
      #`(add x 10))
    (lines-string
      "uint8_t x;"
      "x += 10;")))

(check
  (equal?
    (syntax-c $lookup
      #`(var u8 x)
      #`(set x 10)
      #`(add x 20)
      #`(sub x 30)
      #`(and x 40)
      #`(or x 50)
      #`(xor x 60))
    (lines-string
      "uint8_t x;"
      "x = 10;"
      "x += 20;"
      "x -= 30;"
      "x &= 40;"
      "x |= 50;"
      "x ^= 60;")))

(check
  (equal?
    (syntax-c $lookup
      #`(var u8 x)
      #`(if x (set x 10)))
    (lines-string
      "uint8_t x;"
      "if (x) {"
      "  x = 10;"
      "}")))

(check
  (equal?
    (syntax-c $lookup
      #`(var u8 x)
      #`(if x
        (set x 10)
        (set x 20)))
    (lines-string
      "uint8_t x;"
      "if (x) {"
      "  x = 10;"
      "}"
      "else {"
      "  x = 20;"
      "}")))

(check
  (equal?
    (syntax-c $lookup
      #`(var u8 x)
      #`(while x
          (add x 1)
          (add x 2)))
    (lines-string
      "uint8_t x;"
      "while (x) {"
      "  x += 1;"
      "  x += 2;"
      "}")))

(check
  (equal?
    (syntax-c $lookup
      #`(var u8 x)
      #`(add x (and (- (+ x 10) 20) #xff)))
    (lines-string
      "uint8_t x;"
      "x += (((x + 10) - 20) & 255);")))

(check
  (equal?
    (syntax-c $lookup #`(macro 10 (macro 20 30)))
    (lines-string "macroed(10, macroed(20, 30));")))

(check
  (equal?
    (syntax-c $lookup #`(printf "%i\\n" 10))
    (lines-string "printf(\"%i\\n\", 10);")))

(check
  (equal?
    (syntax-c $lookup #`(SDL_CopyTexture texture (SDL_Rect 0 0 20 30)))
    (lines-string "SDL_CopyTexture(texture, SDL_Rect(0, 0, 20, 30));")))
