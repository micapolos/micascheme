(import
  (scheme)
  (check)
  (stack)
  (reader))

(check-reader-gets! list-reader "foo" (list #\f #\o #\o))
