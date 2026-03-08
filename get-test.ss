(import (scheme) (check) (get))

(check-gets eof?-getter "" #t 0)
(check-gets eof?-getter "abc" #f 0)

(check-gets char?-getter "" #f 0)
(check-gets char?-getter "abc" #\a 1)

(check-gets char-getter "" (eof-object) 1)
(check-gets char-getter "abc" #\a 1)

(check-gets peek-char-getter "" (eof-object) 0)
(check-gets peek-char-getter "abc" #\a 0)

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

(check-gets (test?-string-getter char-alphabetic?) "" "" 0)
(check-gets (test?-string-getter char-alphabetic?) "foo" "foo" 3)
(check-gets (test?-string-getter char-alphabetic?) "foo123" "foo" 3)

(check-gets bfp-getter "" 0 0)
(check-gets
  (append-getter char-getter char-getter bfp-getter char-getter)
  "abcd"
  '(#\a #\b 2 #\c)
  3)

; uses (source-file-descriptor "test.txt" 0)
(check-gets (apply-getter source-file-descriptor-path sfd-getter) "" "test.txt" 0)
(check-gets (apply-getter source-file-descriptor-checksum sfd-getter) "" 0 0)

