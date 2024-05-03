(import (scheme) (syntax) (transformer) (check))

(check
  (equal?
    (syntax->datum
      (replace-identifiers #'___ ellipsis #'($x ___ $y (___ ()))))
    '($x ... $y (... ()))))
