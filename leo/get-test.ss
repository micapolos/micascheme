(import (micascheme) (get) (leo get))

(check-gets atom/eof-getter "" (eof-object) 0)
(check-gets atom/eof-getter "foo" 'foo 3)
(check-gets atom/eof-getter "foo bar" 'foo 3)
(check-gets atom/eof-getter "foo-bar! bar" 'foo-bar! 8)
(check-gets atom/eof-getter "foo: bar" 'foo: 4)
(check-gets atom/eof-getter "123 bar" 123 3)
(check-gets atom/eof-getter "+123 bar" +123 4)
(check-gets atom/eof-getter "-123 bar" -123 4)
(check-gets atom/eof-getter "3.14 bar" 3.14 4)
(check-gets atom/eof-getter "\"foo\" bar" "foo" 5)

(check-get-raises atom/eof-getter " foo bar")
(check-get-raises atom/eof-getter "() bar")
(check-get-raises atom/eof-getter "(foo) bar")
(check-get-raises atom/eof-getter "#t bar")

(check-gets line/eof-getter "" (eof-object) 0)
(check-gets line/eof-getter "123\n" 123 4)
(check-gets line/eof-getter "\"foo\"\n" "foo" 6)
(check-gets line/eof-getter "foo\n" 'foo 4)
(check-gets line/eof-getter "foo bar\n" '(foo bar) 8)
(check-gets line/eof-getter "foo bar goo\n" '(foo (bar goo)) 12)

(check-get-raises line/eof-getter "123 ")

;(check-gets script-getter "123\n" '(123) 0)
