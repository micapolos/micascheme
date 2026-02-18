(import
  (leo2 expand)
  (prefix (leo2 comptime) %)
  (prefix (leo2 term) %%))

(check-expanded=?
  a-type
  (expanded (%type 1) a-type))

; (check-expanded=?
;   a-boolean
;   (expanded (%type 0) (native 'a-boolean)))

; (check-expanded=?
;   (native a-boolean #t)
;   (expanded
;     (%%native (%quote a-boolean))
;     (native #t)))
