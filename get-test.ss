(import (scheme) (check) (get))

(check-gets
  eof?-getter
  ""
  (values #t 0))

(check-gets
  eof?-getter
  "abc"
  (values #f 0))

(check-gets
  char?-getter
  ""
  (values #f 0))

(check-gets
  char?-getter
  "abc"
  (values #\a 1))

(check-gets
  char-getter
  ""
  (values (eof-object) 1))

(check-gets
  char-getter
  "abc"
  (values #\a 1))

(check-gets
  (append-getter char-getter char-getter)
  "abc"
  (values '(#\a #\b) 2))

(check-gets
  (append-getter char-getter (char-ungetter #\a)) "abc"
  (values '(#\a #\a) 0))

(check-gets
  (append-getter char-getter (char-ungetter #\a) char-getter) "abc"
  (values '(#\a #\a #\a) 1))

(check-gets
  datum-getter "(foo bar) (zoo zar)"
  (values '(foo bar) 9))

(check-gets
  (append-getter datum-getter datum-getter)
  "(foo bar) (zoo zar)"
  (values '((foo bar) (zoo zar)) 19))
