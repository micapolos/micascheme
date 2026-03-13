(import
  (micascheme)
  (getter)
  (leo getter)
  (leo load))

(displayln "--- leo/sandbox.leo")
(pretty-print
  (getter-load! lines-getter "leo/sandbox.leo"))

(displayln "--- leo/samples/example.leo")
(pretty-print
  (getter-load! lines-getter "leo/samples/example.leo"))

(displayln "--- leo/samples/library.leo")
(pretty-print
  (getter-load! line-getter "leo/samples/library.leo"))

(displayln "--- leo/samples/program.leo")
(pretty-print
  (getter-load! lines-getter "leo/samples/program.leo"))

(displayln "--- leo/mica/reader/identifier.leo")
(pretty-print
  (getter-load! lines-getter "leo/mica/reader/identifier.leo"))

(displayln "--------------------------------------------")
(eval
  (getter-load! line-annotation-getter "leo/samples/library.leo")
  (copy-environment (environment '(leo scheme)) #t))

(displayln "--- load leo/samples/program.leo")
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
