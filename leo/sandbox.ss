(import
  (micascheme)
  (getter)
  (leo getter)
  (leo load))

(displayln "--- leo/examples/program.leo")
(pretty-print (getter-load! lines-getter "leo/examples/program.leo"))
