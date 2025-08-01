(import
  (asm lang)
  (asm run))

(define-fragment empty)

(define-fragment dw-empty (dw empty))

; TODO: This does not work. Why?!?!
(run
  (dw empty)
  (dw dw-empty))
