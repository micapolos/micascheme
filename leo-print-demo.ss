(import (leo-print))

(display-string "=== Primitive lines ===\n")
(println-leo-line #t)
(println-leo-line #f)
(println-leo-line 123)
(println-leo-line #\a)
(println-leo-line #\space)
(println-leo-line "foo")
(println-leo-line 'foo)

(display-string "=== Leo lines ===\n")
(println-leo-line (() foo))
(println-leo-line (() . 'foo))
(println-leo-line (() . `foo))
(println-leo-line (() . ,foo))
(println-leo-line (() . ,@foo))
(println-leo-line (() . #'foo))
(println-leo-line (() . #`foo))
(println-leo-line (() . #,@foo))
(println-leo-line (() foo (() . (bar))))
(println-leo-line (() foo (() . (bar (() . (goo))))))

(display-string "=== Leo script ===\n")
(print-leo
  10
  foo
  "bar"
  #\A
  #\newline
  #t
  #f
  (void)
  (ld a h)
  (ret)
  (lambda (a) a)
  (() void)
  (() x 10)
  (() radius (() length 10))
  (() point (() x 10) (() y 20))
  (() circle
    (() radius 10)
    (() center
      (() point
        (() x 20)
        (() y 30))))
  (() quote foo)
  (() quasiquote foo)
  (() unquote foo)
  (() unquote-splicing foo)
  (() quasiquote (() foo (() unquote bar)))
  (() quasiquote (() foo (() unquote-splicing (1 2 3))))
  (() quasiquote (() foo (() unquote-splicing (() 1 2 3))))
  (() syntax foo)
  (() quasisyntax foo)
  (() unsyntax foo)
  (() unsyntax-splicing foo)
  (() quasisyntax (() foo (() unsyntax bar)))
  (() quasisyntax (() foo (() unsyntax-splicing bar)))
)
