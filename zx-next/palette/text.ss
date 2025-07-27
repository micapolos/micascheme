(library (zx-next palette text)
  (export copy-text)
  (import (zx-next core))

  (proc copy-text
    (input
      (hl src-text-palette 2 arrays of 8 rgb-333 colors)
      (de dst-palette 256 rgb-333 colors))
    (ld c 0)
    outer-loop
    (ld b 0)
    inner-loop
    (ld a b)
    (call copy-color)
    (ld a c)
    (call copy-color)
    (ld a b)
    (add 8)
    (call copy-color)
    (ld a c)
    (add 8)
    (call copy-color)
    (ld a b)
    (inc a)
    (and #x07)
    (ld b a)
    (jp nz inner-loop)
    (ld a c)
    (inc a)
    (and #x07)
    (ld c a)
    (jp nz outer-loop)
    (ret))

  (proc copy-color
    (input
      (hl src-palette)
      (de dst)
      (a color)
    (output
      (de advanced))
    (preserve (hl bc)
      (add hl a)
      (add hl a)
      (ldi)
      (ldi))
    (ret)))
)
