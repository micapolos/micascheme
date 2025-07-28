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
    (row-size (* width tile-size))
    (tile-map-size (* width height tile-size))
    (tile-defs (+ tile-map tile-map-size))
    (glyph-count 96)
    (glyph-size 8))

  (define-fragments
    (hello-world (dz "Hello, world!"))
    (data (db 0 0 #b11100000))  ; row / col / attr
    (cursor-col-row (db 0 0))
    (attr (db #b11100000)))

  (define-fragment put-char
    (sub #x20)
    (ld hl (cursor-addr))
    (ld (hl) a)
    (inc hl)
    (ld (hl) #b01000000)
    (inc hl)
    (ld (cursor-addr) hl)
    (ret))

  (define-fragment terminal-init
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

    (ret))

  (define-fragment move-to
    (input (h col) (l row))
    (ld (cursor-row-col) hl)
    (ret))

  (define-fragment put-char
    (ld hl (cursor-row-col))
    (ld d l)
    (ld e row-size)
    (mul d e)
    (ld a h)
    (add de a)
    (add de #x4000)
    (ld (de) a)
    (ld a (attr))
    (ld (de) a)
    (ret))

  (define-fragment inc-row
    (ld hl cursor-row-col)
    (ld a (hl))
    (inc a)
    (cp width)
    (if m
      (then (rcf))
      (else (ld a 0) (scf)))
    (ld (hl) a)
    (ret))

  (define-fragment inc-col
    (ld hl (+ cursor-row-col 1))
    (ld a (hl))
    (inc a)
    (cp height)
    (if m
      (then (rcf))
      (else (scf)))
    (ld (hl) a)
    (ret))

  (define-fragment inc-row-col
    (call inc-row)
    (ret nc)
    (jp inc-col))

  (define-fragment advance
    (inc-row-col)
    (call inc-row)
    (ret nc)
    (jp inc-col))

  (define-fragment scroll-up
    ; move one row up
    (ld hl (+ tile-map row-size))
    (ld de tile-map)
    (ld bc (* (- height 1) row-size))
    (ldir)
    ; clear last row
    (ld hl (+ tilemap (* row-size (- height 1))))
    (ld b width)
    (ld a (attr))
    (loop-djnz
      (ld (hl) 0)  ; space
      (inc hl)
      (ld (hl) a)
      (inc hl))
    (ret))
)
