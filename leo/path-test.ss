(import (micascheme) (leo path))

(check (equal? leo-extension "leo"))
(check (equal? (leo-path (list "foo" "bar")) "foo/bar.leo"))
(check (path-leo? "foo/bar.leo"))
(check (not (path-leo? "foo/bar.ss")))
