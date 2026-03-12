(import
  (micascheme)
  (getter)
  (leo getter)
  (leo load))

(displayln "--------------------------------------------")
(pretty-print
  (getter-load! lines-getter "leo/samples/example.leo"))

(displayln "--------------------------------------------")
(pretty-print
  (getter-load! line-getter "leo/samples/library.leo"))

(displayln "--------------------------------------------")
(pretty-print
  (getter-load! lines-getter "leo/samples/program.leo"))

(displayln "--------------------------------------------")
(pretty-print
  (getter-load! lines-getter "leo/mica/parser/identifier.leo"))

(displayln "--------------------------------------------")
(eval
  (getter-load! line-annotation-getter "leo/samples/library.leo")
  (copy-environment (environment '(leo scheme)) #t))

(displayln "--------------------------------------------")
(load-leo-program "leo/samples/program.leo")

(displayln "--------------------------------------------")
(pretty-print
  (eval
    (getter-load! line-annotation-getter "leo/samples/expression.leo")))

(displayln "--------------------------------------------")
(pretty-print
  (eval
    (getter-load! line-annotation-getter "leo/samples/if-expression.leo")
    (environment '(micascheme))))
