(import
  (micascheme)
  (getter)
  (leo getter)
  (leo load))

(display "--- leo/examples/program.leo")
(pretty-print (getter-load! lines-getter "leo/examples/program.leo"))
