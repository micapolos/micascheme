(import (micascheme) (simplang lang))

(check (equal? (simplang (+ "foo" "bar")) "foobar"))
