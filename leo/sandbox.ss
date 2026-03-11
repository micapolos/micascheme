(import
  (micascheme)
  (getter)
  (leo getter)
  (leo load))

(pretty-print
  (getter-load! lines-getter "leo/samples/example.leo"))

(pretty-print
  (getter-load! line-getter "leo/samples/library.leo"))

(pretty-print
  (getter-load! lines-getter "leo/samples/program.leo"))

; TODO: Implement character literal!!!
; (pretty-print
;   (getter-load! lines-getter "leo/samples/parser.leo"))

(eval
  (getter-load! line-annotation-getter "leo/samples/library.leo")
  (copy-environment (environment '(leo lang)) #t))

(load-leo-program "leo/samples/program.leo")

(pretty-print
  (eval
    (getter-load! line-annotation-getter "leo/samples/expression.leo")))

(pretty-print
  (eval
    (getter-load! line-annotation-getter "leo/samples/if-expression.leo")
    (environment '(micascheme))))
