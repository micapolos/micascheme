(import
  (prefix (micascheme) %)
  (only (micascheme) quote quasiquote unquote unquote-splicing)
  (mica reader)
  (leo mica reader quoted))

(check-reader
  (depth->unquoted 0 alphabetic-char)
  (ok "a" #\a)
  (error "'")
  (error "a'")
  (error "ab")
  (error "1'"))

(check-reader
  (depth->unquoted 1 alphabetic-char)
  (ok "a'" #\a)
  (error "'")
  (error "a")
  (error "ab")
  (error "1"))

(check-reader
  (depth->unquoted 2 alphabetic-char)
  (ok "a''" #\a)
  (error "'")
  (error "a")
  (error "a'")
  (error "a'''")
  (error "ab''")
  (error "1"))

(check-reader
  (map
    (begin-quoted-annotation (annotation numeric-char))
    %annotation-stripped)
  (ok "1" #\1)
  (ok "'1" '`#\1)
  (ok "''1" '``#\1)
  (error "`1"))

(check-reader
  (end-quoted-annotations null)
  (ok "" '())
  (ok "'" '())
  (ok "'..." '())
  (ok "''..." '()))

(check-reader
  (map
    (end-quoted-annotations
      (list (annotation alphabetic-char)))
    (%lambda ($annotation)
      (%map %annotation-stripped $annotation)))
  (ok "a" '(#\a))
  (ok "'a" '(,#\a))
  (ok "'...a" '(,@#\a))
  (ok "''...a" '(,,@#\a)))

(check-reader
  (map
    (end-quoted-annotations
      (list
        (annotation alphabetic-char)
        (annotation numeric-char)))
    (%lambda ($annotation)
      (%map %annotation-stripped $annotation)))
  (ok "a1" '(#\a #\1))
  (ok "'a1" '(,#\a ,#\1))
  (ok "'...a1" '(,@#\a ,@#\1))
  (ok "''...a1" '(,,@#\a ,,@#\1)))
