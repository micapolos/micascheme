(import (micascheme) (html))

(writeln
  (html-string
    (html
      (title "My page")
      (link
        (with
          (href "tspl.css")
          (rel "stylesheet")
          (type "text/css")))
      (body
        (h1 "Hello, world!")
        (p "This is my paragraph")))))
