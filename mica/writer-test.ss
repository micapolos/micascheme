(import (scheme) (check) (list) (mica writer))

(check-writer null-writer
  (writes null "")
  (rejects "foo"))

(check-writer (writer #\a)
  (writes #\a "a")
  (rejects #\b))

(check-writer (writer "foo")
  (writes "foo" "foo")
  (rejects "bar"))

(check-writer char-writer
  (writes #\a "a")
  (writes #\1 "1")
  (rejects "a"))

(check-writer string-writer
  (writes "foo" "foo")
  (writes "bar" "bar")
  (rejects 123))

(check-writer number-writer
  (writes 123 "123")
  (rejects "foo"))

(check-writer (writer-filter odd? number-writer)
  (writes 123 "123")
  (rejects 124)
  (rejects "foo"))

(check-writer (writer-map integer? integer->char char-writer)
  (writes 32 " ")
  (rejects "foo"))

(check-writer (or-writer)
  (rejects 123))

(check-writer (or-writer char-writer)
  (writes #\a "a")
  (rejects "foo" 123))

(check-writer (or-writer char-writer string-writer)
  (writes #\a "a")
  (writes "foo" "foo")
  (rejects 123))

(check-writer (or-writer char-writer string-writer number-writer)
  (writes #\a "a")
  (writes "foo" "foo")
  (writes 123 "123")
  (rejects #f))
