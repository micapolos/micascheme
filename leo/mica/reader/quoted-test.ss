(import
  (prefix (micascheme) %)
  (only (micascheme) quote)
  (mica reader)
  (leo mica reader quoted))

(check-reader
  (map
    (begin-quoted-annotation (annotation numeric-char))
    %annotation-stripped)
  (ok "1" #\1)
  (ok "'1" ''#\1)
  (ok "''1" '''#\1)
  (ok "`1" '`#\1)
  (ok "``1" '``#\1))

(check-reader
  (map
    (end-quoted-annotation (annotation numeric-char))
    %annotation-stripped)
  (ok "1" #\1)
  (ok "`1" ',#\1)
  (ok "`...1" ',@#\1)
  (ok "``1" ',,#\1)
  (ok "`...`...1" ',@,@#\1))

(check-reader
  (map
    (end-quoted-annotations
      (list-with (annotation alphabetic-char)))
    (%lambda ($annotations)
      (%map %annotation-stripped $annotations)))
  (ok "a" '(#\a))
  (ok "`a" '((unquote #\a)))
  (ok "`...a" '((unquote-splicing #\a)))
  (ok "``...a" '((unquote (unquote-splicing #\a)))))

(check-reader
  (map
    (end-quoted-annotations
      (list-with
        (annotation alphabetic-char)
        (annotation numeric-char)))
    (%lambda ($annotations)
      (%map %annotation-stripped $annotations)))
  (ok "a1" '(#\a #\1))
  (ok "`a1" '((unquote #\a #\1)))
  (ok "`...a1" '((unquote-splicing #\a #\1)))
  (ok "``...a1" '((unquote (unquote-splicing #\a #\1)))))
