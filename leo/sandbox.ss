(import
  (micascheme)
  (getter)
  (leo getter))

; (pretty-print
;   (getter-load! lines-getter "leo/samples/example.leo"))

(pretty-print
  (eval
    (getter-load! line-getter "leo/samples/expression.leo")))

(pretty-print
  (eval
    (getter-load! line-getter "leo/samples/if-expression.leo")))
