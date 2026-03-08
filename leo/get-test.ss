(import (micascheme) (get) (leo get))

(check (raises (get symbol-getter "")))

(check-gets symbol-getter "foo"
  (values 'foo 3))

(check-gets symbol-getter "foo("
  (values 'foo 3))

(check-gets symbol-getter "foo bar"
  (values 'foo 3))
