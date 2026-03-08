(import (micascheme) (get) (leo get))

(check-gets word?-getter "" #f 0)
(check-gets word?-getter "(foo)" #f 0)
(check-gets word?-getter "foo" 'foo 3)
(check-gets word?-getter "foo(" 'foo 3)
(check-gets word?-getter "foo bar" 'foo 3)

(check-gets atom?-getter "" (eof-object) 0)
(check-gets atom?-getter "foo bar" 'foo 3)
(check-gets atom?-getter "123 bar" 123 3)
(check-gets atom?-getter "3.14 bar" 3.14 4)
(check-gets atom?-getter "\"foo\" bar" "foo" 5)

(check-gets atom?-getter " foo bar" #f 0)
(check-gets atom?-getter "() bar" #f 0)
(check-gets atom?-getter "(foo) bar" #f 0)
(check-gets atom?-getter "#t bar" #f 0)
