(library (zx-next lookup)
  (export
    lookup
    lookup-7

    lookup-byte
    lookup-word
    lookup-word-7)
  (import (zx-next core))

  (define-ops (keywords a de)
    ((lookup a)
      (add hl a)
      (ld a (hl)))
    ((lookup-7 de)
      (add a)
      (add hl a)
      (ld e (hl))
      (inc hl)
      (ld d (hl)))
    ((lookup de)
      (ld d 0)
      (ld e a)
      (add hl de)
      (add hl de)
      (ld e (hl))
      (inc hl)
      (ld d (hl))))

  (define-fragment lookup-byte
    (input (a index) (hl table))
    (output (a value))
    (lookup a)
    (ret))

  (define-fragment lookup-word
    (input (a index) (hl table))
    (output (de value))
    (lookup de)
    (ret))

  (define-fragment lookup-word-7
    (input (a index) (hl table))
    (output (de value))
    (lookup-7 de)
    (ret))
)
