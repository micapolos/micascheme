(import (scheme) (check) (list) (mica writer))

(check-writer null-writer
  (writes null "")
  (raises "foo"))

(check-writer (writer #\a)
  (writes #\a "a")
  (raises #\b))

(check-writer (writer "foo")
  (writes "foo" "foo")
  (raises "bar"))

(check-writer char-writer
  (writes #\a "a")
  (writes #\1 "1")
  (raises "a"))

(check-writer (?char-writer char-numeric?)
  (writes #\1 "1")
  (raises #\a)
  (raises "a"))

(check-writer string-writer
  (writes "foo" "foo")
  (writes "bar" "bar")
  (raises 123))

(check-writer (or-writer)
  (raises 123))

(check-writer (or-writer char-writer string-writer)
  (writes #\a "a")
  (writes "foo" "foo")
  (raises 123))
