(import (micascheme) (check) (html))

(check
  (equal?
    (html-string
      (html
        (head
          (title "My page")
          (meta (with (charset "UTF-8"))))
        (body
          (h1 "Hello, world!")
          (p "This is my paragraph"))))
    "<!DOCTYPE html><html><head><title>My page</title><meta charset=\"UTF-8\"/></head><body><h1>Hello, world!</h1><p>This is my paragraph</p></body></html>"))
