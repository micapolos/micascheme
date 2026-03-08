(import (micascheme) (get) (leo get))

(check (raises (get word-getter "")))

(check-gets word-getter "foo"
  (values 'foo 3))

(check-gets word-getter "foo("
  (values 'foo 3))

(check-gets word-getter "foo bar"
  (values 'foo 3))
