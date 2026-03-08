(import (micascheme) (get) (leo get))

(check-gets word?-getter ""
  (values #f 0))

(check-gets word?-getter "foo"
  (values 'foo 3))

(check-gets word?-getter "foo("
  (values 'foo 3))

(check-gets word?-getter "foo bar"
  (values 'foo 3))
