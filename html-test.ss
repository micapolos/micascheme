(import (micascheme) (check) (html))

(check
  (equal?
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
          (p "This is my paragraph"))))
    "<html><title>My page</title><link href=\"tspl.css\" rel=\"stylesheet\" type=\"text/css\"></link><body><h1>Hello, world!</h1><p>This is my paragraph</p></body></html>"))
