(import (micascheme) (tico path))

(check
  (equal?
    (path foo bar)
    (make-path 'foo (make-path 'bar #f))))

(check
  (equal?
    (path-filename (path foo bar))
    "foo/bar.leo"))
