(import
  (tt lang)
  (tt procedure)
  (tt string))

(check
  (string=?
    ((partial string+) "foo" "bar")
    "foobar"))

(check
  (string=?
    ((partial string+ "foo") "bar")
    "foobar"))

(check
  (string=?
    ((partial string+ "foo" "bar"))
    "foobar"))

(check (fails (partial string+ 10)))
(check (fails (partial string+ "foo" 10)))
(check (fails (partial string+ "foo" "bar" "zoo")))
