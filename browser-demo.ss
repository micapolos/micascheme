(import (micascheme) (browser) (html))

(browse
  (html
    (head
      (meta (with (charset "UTF-8")))
      (title "Hello!"))
    (body
      (h1 "Pozdrowienia dla Marcina!")
      (p
        "Życzę rychłego powrotu do zdrowia... "
        "a w wolnej chwili rzucenia okiem na "
        (a
          (with (href "https://github.com/cisco/ChezScheme"))
          (b "ChezScheme"))
        "."))))
