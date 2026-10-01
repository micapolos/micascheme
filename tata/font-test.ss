(import
  (scheme)
  (check)
  (tata font)
  (lets)
  (procedure))

(lets
  ($font
    (call-with-port
      (open-file-input-port "tata/mica.font" (file-options))
      read-font))
  (run
    (check (= (font-height $font) 8))
    (check (= (font-space-width $font) 2))
    (check (= (font-glyph-spacing $font) 1))
    (check (= (font-line-spacing $font) 1))
    (check (= (vector-length (font-glyph-vector $font)) 94))
    (check (font-glyph? $font #\a))
    (check (not (font-glyph? $font #\newline)))))

(lets
  ($font
    (call-with-port
      (open-file-input-port "tata/kora.font" (file-options))
      read-font))
  (run
    (check (= (font-height $font) 9))
    (check (= (font-space-width $font) 2))
    (check (= (font-glyph-spacing $font) 1))
    (check (= (font-line-spacing $font) 1))
    (check (= (vector-length (font-glyph-vector $font)) 94))
    (check (font-glyph? $font #\a))
    (check (not (font-glyph? $font #\newline)))))
