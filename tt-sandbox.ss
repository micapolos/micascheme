(import (tt))

(print
  (string-join
    (link
      "numbers: "
      (intercalate
        (map number->string
          (list 1 2 3 4 5 6))
        ", "))))
