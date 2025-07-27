(library (zx-next palette text)
  (export text-palette->palette)
  (import
    (zx-next core)
    (zx-next rgb))

  (block text-palette->palette
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

  (block copy-color
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

  (block text-palette-default
    normal
    (rgb-333 0 0 1)
    (rgb-333 0 0 6)
    (rgb-333 0 5 1)
    (rgb-333 0 5 6)
    (rgb-333 5 0 1)
    (rgb-333 5 0 6)
    (rgb-333 5 5 1)
    (rgb-333 5 5 6)
    bright
    (rgb-333 0 0 1)
    (rgb-333 0 0 7)
    (rgb-333 0 7 1)
    (rgb-333 0 7 7)
    (rgb-333 7 0 1)
    (rgb-333 7 0 7)
    (rgb-333 7 7 1)
    (rgb-333 7 7 7))
)
