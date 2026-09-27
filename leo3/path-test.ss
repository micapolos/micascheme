(import
  (scheme)
  (check)
  (leo3 path))

(check (path-leo? "foo.leo"))
(check (not (path-leo? "foo.ss")))
(check (not (path-leo? "foo")))
