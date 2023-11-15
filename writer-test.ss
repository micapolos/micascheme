(import
  (micascheme)
  (writer))

(check
  (equal?
    (do-writer-string $writer (writer-write-string $writer "foo"))
    "foo"))
