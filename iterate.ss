(library (iterate)
  (export iterate)
  (import (scheme))

  (define (iterate $proc $item $count)
    (cond
      ((= $count 0) $item)
      (else (iterate $proc ($proc $item) (- $count 1)))))
)
