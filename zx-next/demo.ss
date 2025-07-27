(import
  (zx-next core)
  (zx-next debug)
  (zx-next mem))

(run
  ;(nextreg #x6b #b11001011)  ; enable tilemap, 80x32, 512 tiles, textmode, tilemap over ULA
  ;(nextreg #x6c #b00000000)  ; Default tilemap attribute
  ;(nextreg #x6e (fxsrl (fx- tileMap #x4000) 8))
  ;(nextreg #x6f (fxsrl (fx- tileDefs #x4000) 8))
  ;(nextreg #x68 #b10010000)  ; ulaOff, extKeysOff

  (ld de #x4000)
  (ld bc #x1800)
  (ld a #b11110000)
  (call mem-fill)

  (ld bc #x0100)
  (ld a #b01111000)
  (call mem-fill)

  (ld bc #x0100)
  (ld a #b11111011)
  (call mem-fill)

  (ld bc #x0100)
  (ld a #b01110101)
  (call mem-fill)

  (call loop-bars))
