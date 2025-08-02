(library (zx-next terminal)
  (export
    terminal-init
    terminal-move-to
    terminal-write-char
    terminal-newline
    terminal-wait-space)
  (import
    (zx-next core)
    (zx-next mem)
    (zx-next font topaz-8)
    (zx-next palette text)
    (zx-next palette)
    (zx-next write)
    (zx-next tile map)
    (zx-next tile coord)
    (zx-next debug))

  (define-values
    (width 80)
    (height 32)
    (tile-map #x4000)
    (tile-size 2)
    (terminal-size #x2050)
    (row-size (* width tile-size))
    (tile-map-size (* width height tile-size))
    (tile-defs (+ tile-map tile-map-size))
    (glyph-count 96)
    (glyph-size 8))

  (define-fragments
    (cursor-coord (dw #x0000))
    (attr (db #b11100000)))

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

    (ld hl terminal-write-char)
    (call write-init)

    (ret))

  (define-fragment terminal-move-to
    (input (hl row col))
    (ex de hl)
    (ld hl cursor-coord)
    (ld (hl) e)
    (inc hl)
    (ld (hl) d)
    (ret))

  (define-fragment terminal-newline
    (ld hl cursor-coord)
    (ld (hl) 0)
    (inc hl)
    (ld a (hl))
    (inc a)
    (cp height)
    (when nz
      (ld (hl) a)
      (ret))
    (jp terminal-scroll-up))

  (define-fragment terminal-write-char
    (cp #x0d)
    (jp z terminal-newline)

    (sub #x20)
    (ret m)

    (ld hl cursor-coord)

    ; hl = cursor coord
    (ld e (hl))
    (inc hl)
    (ld d (hl))
    (ex de hl)

    (preserve (de hl)
      (preserve (af)
        (ld de terminal-size)
        (call tile-coord-index)
        (add hl hl)
        (add hl tile-map))

      ; put char and attr
      (ld (hl) a)
      (inc hl)
      (ld a (attr))
      (ld (hl) a))

    ; inc cursor coord
    (preserve (de)
      (ld de terminal-size)
      (call tile-coord-inc))

    ; check for overflow
    (ld a h)
    (or a)
    (when z
      (ld a l)
      (or a)
      (when z
        (ld h (- height 1))
        (preserve (de hl)
          (call terminal-scroll-up))))

    (ex de hl)
    (ld (hl) d)
    (dec hl)
    (ld (hl) e)

    (ret))

  (define-fragment terminal-advance
    (ld hl cursor-coord)

    ; hl = cursor coord
    (ld e (hl))
    (inc hl)
    (ld d (hl))
    (ex de hl)

    ; inc cursor coord
    (preserve (de)
      (ld de terminal-size)
      (call tile-coord-inc))

    ; check for overflow
    (ld a h)
    (or a)
    (when z
      (ld a l)
      (or a)
      (when z
        (ld h (- height 1))
        (preserve (de hl)
          (call terminal-scroll-up))))

    (ex de hl)
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

  (define-fragment press-space-string (dz "=== PRESS SPACE ==="))

  (define-fragment terminal-wait-space
    (ld hl press-space-string)
    (call writeln-string)
    (call wait-space))
)
