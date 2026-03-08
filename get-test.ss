(import (scheme) (check) (get))

(check-gets eof?-getter "" #t 0)
(check-gets eof?-getter "abc" #f 0)

(check-gets char?-getter "" #f 0)
(check-gets char?-getter "abc" #\a 1)
(check-gets char-getter "" (eof-object) 1)
(check-gets char-getter "abc" #\a 1)

(check-gets
  (append-getter char-getter char-getter)
  "abc"
  '(#\a #\b)
  2)

(check-gets
  (append-getter char-getter (char-ungetter #\a))
  "abc"
  '(#\a #\a)
  0)

(check-gets
  (append-getter char-getter (char-ungetter #\a) char-getter)
  "abc"
  '(#\a #\a #\a)
  1)

(check-gets datum-getter "(foo bar) (zoo zar)" '(foo bar) 9)
