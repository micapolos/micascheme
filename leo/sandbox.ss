(import
  (micascheme)
  (getter)
  (leo getter))

(pretty-print
  (getter-load! lines-getter "leo/samples/example.leo"))

(pretty-print
  (getter-load! line-getter "leo/samples/library.leo"))

(pretty-print
  (getter-load! lines-getter "leo/samples/program.leo"))

(eval
  (getter-load! line-annotation-getter "leo/samples/library.leo")
  (copy-environment (environment '(leo lang)) #t))

(import (leo term))
(pretty-print (native 10))
(pretty-print (application (native 20) (native 30)))
(pretty-print (abstraction (lambda (x) x)))
(pretty-print hello-world)

(pretty-print
  (eval
    (getter-load! line-annotation-getter "leo/samples/expression.leo")))

(pretty-print
  (eval
    (getter-load! line-annotation-getter "leo/samples/if-expression.leo")
    (environment '(micascheme))))
