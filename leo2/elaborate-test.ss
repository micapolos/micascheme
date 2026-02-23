(import
  (leo2 base)
  (leo2 term)
  (leo2 elaborate))

(check-elaborates
  (type 12)
  (type 12))

; (check-elaborates
;   (signature (type 0)
;     (lambda (x) x))
;   (typed
;     (signature (type 0)
;       (lambda (type 0)))
;     (signature (type 0)
;       (lambda (typed anything (variable 0))))))
