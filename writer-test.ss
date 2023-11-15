(import
  (micascheme)
  (writer))

(check
  (equal?
    (writer-value (writer-write-string (chars-writer) "foo"))
    (stack #\f #\o #\o)))
