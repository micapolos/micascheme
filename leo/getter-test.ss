(import
  (micascheme)
  (getter)
  (leo getter))

(check-get-raises atom-getter "")
(check-gets atom-getter "foo" 'foo 3)
(check-gets atom-getter "foo bar" 'foo 3)
(check-gets atom-getter "123 bar" 123 3)
(check-gets atom-getter "\"foo\" bar" "foo" 5)

;(check-get-raises atom-getter "+123 bar")
;(check-get-raises atom-getter "-123 bar")
(check-get-raises atom-getter " foo bar")
(check-get-raises atom-getter "() bar")
(check-get-raises atom-getter "(foo) bar")
(check-get-raises atom-getter "#t bar")

; TODO: These should fail
;(check-get-raises atom-getter "3.14 bar")
;(check-get-raises atom-getter "foo-bar! bar")
;(check-get-raises atom-getter "foo: bar")

(check-gets line-getter "123\n" 123 4)
(check-gets line-getter "\"foo\"\n" "foo" 6)
(check-gets line-getter "foo\n" 'foo 4)
(check-gets line-getter "foo bar\n" '(foo bar) 8)
(check-gets line-getter "foo bar goo\n" '(foo (bar goo)) 12)

(check-get-raises line-getter "123 ")

(check-gets lines-getter "" '() 0)
(check-gets lines-getter "10\n" '(10) 3)
(check-gets lines-getter "10\n20\n" '(10 20) 6)
(check-gets lines-getter "foo\nfoo bar\n" '(foo (foo bar)) 12)

(check-gets inline-getter "123" 123 3)
(check-gets inline-getter "\"foo\"" "foo" 5)
(check-gets inline-getter "foo" 'foo 3)
(check-gets inline-getter "foo bar" '(foo bar) 7)
(check-gets inline-getter "foo bar goo" '(foo (bar goo)) 11)

(check-gets inline-getter "123 " 123 3)

;(check-gets inlines-getter "" '() 0)
;(check-gets inlines-getter "10" '(10) 3)
; (check-gets inlines-getter "10, 20" '(10 20) 6)
; (check-gets inlines-getter "foo, foo bar" '(foo (foo bar)) 12)

(check-gets
  line-getter
  (lines-string
    "point"
    "  x 10"
    "  y 20")
  '(point (x 10) (y 20))
  20 3 0)

(check-gets
  line-getter
  (lines-string
    "circle"
    "  center point"
    "    x 10"
    "    y 10"
    "  radius 10")
  '(circle
    (center
      (point
        (x 10)
        (y 10)))
    (radius 10))
  52 5 0)

; TODO: Fixit!!!
; (check-gets
;   line-getter
;   (lines-string
;     "point"
;     ""
;     "  x 10"
;     ""
;     ""
;     "  y 20"
;     ""
;     "")
;   '(point (x 10) (y 20))
;   25 8 0)
