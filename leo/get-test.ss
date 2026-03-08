(import (micascheme) (get) (leo get))

(check-gets word?-getter "" #f 0)
(check-gets word?-getter "(foo)" #f 0)
(check-gets word?-getter "foo" 'foo 3)
(check-gets word?-getter "foo(" 'foo 3)
(check-gets word?-getter "foo bar" 'foo 3)
