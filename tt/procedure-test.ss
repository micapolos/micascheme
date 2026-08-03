(import
  (tt lang)
  (tt procedure)
  (tt string))

(check
  (string=?
    ((partial string-append) "foo" "bar")
    "foobar"))

(check
  (string=?
    ((partial string-append "foo") "bar")
    "foobar"))

(check
  (string=?
    ((partial string-append "foo" "bar"))
    "foobar"))

(check (fails (partial string-append 10)))
(check (fails (partial string-append "foo" 10)))
(check (fails (partial string-append "foo" "bar" "zoo")))
