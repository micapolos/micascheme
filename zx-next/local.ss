(library (zx-next local)
  (export local ld-local ld-arg)
  (import (zx-next core))

  (define-ops (keywords a b c d e bc de)
    ((local body ...)
      (preserve (ix)
        (ld ix 0)
        (add ix sp)
        body ...))

    ((ld-local-r r offset) (ld r (+ ix (+ -1 (- offset)))))
    ((ld-local-r offset r) (ld (+ ix (+ -1 (- offset))) r))

    ((ld-arg-r r offset)   (ld r (+ ix (+ 4 offset))))
    ((ld-arg-r offset r)   (ld (+ ix (+ 4 offset)) r))

    ((ld-local a offset) (ld-local-r a offset))
    ((ld-local b offset) (ld-local-r b offset))
    ((ld-local c offset) (ld-local-r c offset))
    ((ld-local d offset) (ld-local-r d offset))
    ((ld-local e offset) (ld-local-r e offset))

    ((ld-arg a offset) (ld-arg-r a offset))
    ((ld-arg b offset) (ld-arg-r b offset))
    ((ld-arg c offset) (ld-arg-r c offset))
    ((ld-arg d offset) (ld-arg-r d offset))
    ((ld-arg e offset) (ld-arg-r e offset))

    ((ld-local bc offset)
      (ld-local c (+ offset 1))
      (ld-local b (+ offset 0)))

    ((ld-local de offset)
      (ld-local e (+ offset 1))
      (ld-local d (+ offset 0)))

    ((ld-arg bc offset)
      (ld-arg c (+ offset 0))
      (ld-arg b (+ offset 1)))

    ((ld-arg de offset)
      (ld-arg e (+ offset 0))
      (ld-arg d (+ offset 1)))

    ((ld-local offset a) (ld-local-r offset a))
    ((ld-local offset b) (ld-local-r offset b))
    ((ld-local offset c) (ld-local-r offset c))
    ((ld-local offset d) (ld-local-r offset d))
    ((ld-local offset e) (ld-local-r offset e))

    ((ld-arg offset a) (ld-arg-r offset a))
    ((ld-arg offset b) (ld-arg-r offset b))
    ((ld-arg offset c) (ld-arg-r offset c))
    ((ld-arg offset d) (ld-arg-r offset d))
    ((ld-arg offset e) (ld-arg-r offset e))

    ((ld-local offset bc)
      (ld-local (+ offset 1) c)
      (ld-local (+ offset 0) b))

    ((ld-local offset de)
      (ld-local (+ offset 1) e)
      (ld-local (+ offset 0) d))

    ((ld-arg offset bc)
      (ld-arg (+ offset 0) c)
      (ld-arg (+ offset 1) b))

    ((ld-arg offset de)
      (ld-arg (+ offset 0) e)
      (ld-arg (+ offset 1) d)))
)
