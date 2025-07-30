(library (zx-next terminal)
  (export
    terminal-init
    terminal-move-to
    terminal-put-char)
  (import
    (zx-next core)
    (zx-next mem)
    (zx-next font topaz-8)
    (zx-next palette text)
    (zx-next palette)
    (zx-next writer)
    (zx-next tile map)
    (zx-next tile coord))

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
    (cursor-addr (dw #x4000))
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
    (jp palette-load-9bit))

  (define-fragment terminal-move-to
    (input (hl row col))
    ; hl = index
    (ld de #x2050)
    (call tile-coord-index)

    ; hl = address
    (add hl hl)
    (add hl tile-map)

    ; store cursor addr
    (ex de hl)
    (ld hl cursor-addr)
    (ld (hl) e)
    (inc hl)
    (ld (hl) d)

    (ret))

  (define-fragment terminal-put-char
    (ld hl cursor-addr)

    ; de = cursor addr
    (ld e (hl))
    (inc hl)
    (ld d (hl))

    ; put char and attr
    (sub #x20)
    (ld (de) a)
    (inc de)
    (ld a (attr))
    (ld (de) a)
    (inc de)

    ; return to
    (ld a e)
    (cp (fxand (+ tile-map tile-map-size) #xff))
    (when z
      (ld a d)
      (cp (fxsrl (+ tile-map tile-map-size) 8))
      (when z
        (add de (- #x10000 row-size))
        (preserve (de hl) (call terminal-scroll-up))))

    (ld (hl) d)
    (dec hl)
    (ld (hl) e)

    (ret))

  (define-fragment terminal-scroll-up
    ; move one row up
    (ld hl (+ tile-map row-size))
    (ld de tile-map)
    (ld bc (* (- height 1) row-size))
    (ldir)
    ; clear last row
    (ld hl (+ tile-map (* row-size (- height 1))))
    (ld b width)
    (ld a (attr))
    (loop-djnz
      (ld (hl) 0)  ; space
      (inc hl)
      (ld (hl) a)
      (inc hl))
    (ret))
)
