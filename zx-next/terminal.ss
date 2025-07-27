(library (zx-next terminal)
  (export terminal-init)
  (import (zx-next core))

  (proc terminal-init
    (nextreg #x6b #b11001011)  ; enable tilemap, 80x32, 512 tiles, textmode, tilemap over ULA
    (nextreg #x6c #b00000000)  ; Default tilemap attribute
    (nextreg #x6e (fxsrl (fx- tileMap #x4000) 8))
    (nextreg #x6f (fxsrl (fx- tileDefs #x4000) 8))
    (nextreg #x68 #b10010000)  ; ulaOff, extKeysOff

    ; Clear tilemap
    (ld hl tile-map)
    (ld bd tile-map-size)
    (call mem-clear)


    ; Copy font into tile defs
    (ld hl font)
    (ld de tile-defs)
    (ld bc font-size)
    (ldir)

    (ld hl text-palette)
    (ld de tile-map-palette)
    (call text-palette->palette)

    (nextreg #x43 #b00110000)  ; tilemap 1-st palette for write, auto-increment
    (nextreg #x40 0)           ; start palette index = 0

    (ld hl tile-map-palette)
    (ld b 0)
    (call palette-load-9bit)

    (ld hl write-char)
    (ld (writer-char-proc) hl)

    ; Set white color (not bright)
    (preserve (ix)
      (ld ix terminal-printer)
      (ld (+ ix printer-attr) #b11100000))

    (ret))


)
