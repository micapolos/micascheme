(library (zx-next local)
  (export local ld-local)
  (import (zx-next core))

  (define-ops (keywords a b c d e bc de)
    ((local body ...)
      (preserve (ix)
        (ld ix 0)
        (add ix sp)
        body ...))

    ((ld-local a offset) (ld a (+ ix offset)))
    ((ld-local b offset) (ld b (+ ix offset)))
    ((ld-local c offset) (ld c (+ ix offset)))
    ((ld-local d offset) (ld d (+ ix offset)))
    ((ld-local e offset) (ld e (+ ix offset)))

    ((ld-local bc offset)
      (ld-local c (+ offset 1))
      (ld-local b (+ offset 0)))

    ((ld-local de offset)
      (ld-local e (+ offset 1))
      (ld-local d (+ offset 0)))

    ((ld-local offset a) (ld (+ ix offset) a))
    ((ld-local offset b) (ld (+ ix offset) b))
    ((ld-local offset c) (ld (+ ix offset) c))
    ((ld-local offset d) (ld (+ ix offset) d))
    ((ld-local offset e) (ld (+ ix offset) e))

    ((ld-local offset bc)
      (ld-local (+ offset 1) c)
      (ld-local (+ offset 0) b))

    ((ld-local offset de)
      (ld-local (+ offset 1) e)
      (ld-local (+ offset 0) d)))
)
