(import (scheme) (check) (leo3 transformer))

(check-transforms (transform-identifier foo) foo)
(check-transform-raises (transform-identifier 123))

(check-transforms (transform-identifier-pair (foo bar)) (foo bar))
(check-transform-raises (transform-identifier-pair '(foo 123)))

(check-transforms (transform-name (name foo bar)) (foo bar))
(check-transform-raises (transform-name '(nazwa foo bar)))
(check-transform-raises (transform-name '(name foo 123)))

(check-transforms (transform-import-spec tpl (from foo bar)) (foo bar))
(check-transforms (transform-import-spec tpl (only x y (from foo bar))) (only (foo bar) x y))
(check-transforms (transform-import-spec tpl (except x y (from foo bar))) (except (foo bar) x y))
(check-transforms (transform-import-spec tpl (prefix foo-bar (from foo bar))) (prefix (foo bar) foo-bar-))
(check-transforms (transform-import-spec tpl (rename (a b) (c d) (from foo bar))) (rename (foo bar) (a b) (c d)))

(check-transforms
  (transform-import tpl
    (import
      (from foo bar)
      (except x y (from zoo zar))))
  (import
    (foo bar)
    (except (zoo zar) x y)))

(check-transforms
  (transform-export tpl (export x y))
  (export x y))

(check-transforms
  (transform-library tpl
    (library
      (name foo bar)
      (export x y)
      (import
        (from scheme)
        (from goo gar))
      (define x 10)
      (define y 20)))
  (library (foo bar)
      (export x y)
      (import
        (scheme)
        (goo gar))
      (define x 10)
      (define y 20)))
