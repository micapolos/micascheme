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
    (check (font-char-glyph $font #\a))
    (check (raises (font-char-glyph $font #\space)))
    (check (raises (font-char-glyph $font #\newline)))
    (check (font-char-glyph? $font #\a))
    (check (not (font-char-glyph? $font #\space)))
    (check (not (font-char-glyph? $font #\newline)))
    (check (= (font-string-width $font "") 0))
    (check (= (font-string-width $font "A") 4))
    (check (= (font-string-width $font "i") 1))
    (check (= (font-string-width $font "Ai") 6))
    (check (= (font-string-width $font " ") 2))
    (check (= (font-string-width $font "A i") 9))
    (check (raises (font-string-width $font "\n")))))

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
    (check (font-char-glyph? $font #\a))
    (check (not (font-char-glyph? $font #\newline)))))
