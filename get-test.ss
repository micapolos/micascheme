(import (scheme) (check) (get))

(check-gets
  datum-getter "(foo bar) (zoo zar)"
  (values (foo bar) 9))
