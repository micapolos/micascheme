(library (tata glyph)
  (export
    glyph
    glyph?
    glyph-width
    glyph-bytevector
    read-glyph)
  (import
    (scheme)
    (data)
    (lets)
    (procedure)
    (port))

  (data (glyph width bytevector))

  (define (read-glyph $port)
    (lets
      ($width (get-u8-or-throw $port))
      ($bytevector (make-bytevector (fx*/wraparound $width 4)))
      ($index 0)
      (run
        (repeat $width
          (bytevector-u32-set! $bytevector $index (get-u32-or-throw $port) (endianness big))
          (set! $index (fx+/wraparound $index 4))))
      (glyph $width (bytevector->immutable-bytevector $bytevector))))
)
