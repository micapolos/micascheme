(library (zx-next terminal)
  (export terminal-init)
  (import
    (zx-next core)
    (zx-next mem)
    (zx-next font topaz-8)
    (zx-next palette text)
    (zx-next palette)
    (zx-next writer))

  (define-values
    (width 80)
    (height 32)
    (tile-map #x4000)
    (tile-size 2)
    (tile-map-size (* width height tile-size))
    (tile-defs (+ tile-map tile-map-size))
    (glyph-count 96)
    (glyph-size 8))

  (define-fragments
    (hello-world (dz "Hello, world!"))
    (cursor-addr (dw #x4000))
    (put-char
      (sub #x20)
      (ld hl (cursor-addr))
      (ld (hl) a)
      (inc hl)
      (ld (hl) #b01000000)
      (inc hl)
      (ld (cursor-addr) hl)
      (ret))
    (terminal-init
      (nextreg #x6b #b11001011)  ; enable tilemap, 80x32, 512 tiles, textmode, tilemap over ULA
      (nextreg #x6c #b00000000)  ; Default tilemap attribute
      (nextreg #x6e (fxsrl (fx- tile-map #x4000) 8))
      (nextreg #x6f (fxsrl (fx- tile-defs #x4000) 8))
      (nextreg #x68 #b10010000)  ; ulaOff, extKeysOff

      ; Clear tilemap
      (ld de tile-map)
      (ld bc tile-map-size)
      (call mem-clear)

      ; Copy font into tile defs
      (ld hl font-topaz-8)
      (ld de tile-defs)
      (ld bc (* glyph-count glyph-size))
      (ldir)

      (ld hl default-text-palette)
      (ld de #x8000)  ; TODO: Use scratch bank
      (call text-palette->palette)

      (nextreg #x43 #b00110000)  ; tilemap 1-st palette for write, auto-increment
      (nextreg #x40 0)           ; start palette index = 0

      (ld hl #x8000)
      (ld b 0)
      (call palette-load-9bit)

      ; Write all chars
      (ld hl put-char)
      (ld de hello-world)
      (call write-string)
      (ld b glyph-count)

      (ret)))
)
