(import
  (micascheme)
  (tico path)
  (leo reader))

(check
  (equal?
    (path foo bar)
    (make-path 'foo (make-path 'bar #f))))

(check
  (equal?
    (list->path (list 'foo 'bar))
    (path foo bar)))

(check
  (equal?
    (path-filename (path foo bar))
    "foo/bar.leo"))

(check
  (equal?
    (reader-eval (paths-reader)
      a1
      (a2 b1 b2)
      (a3 (b1 c1 c2) (b2 c1 c2)))
    (list
      (path a1)
      (path a2 b1)
      (path a2 b2)
      (path a3 b1 c1)
      (path a3 b1 c2)
      (path a3 b2 c1)
      (path a3 b2 c2))))
