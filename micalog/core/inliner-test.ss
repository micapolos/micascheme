(import
  (micascheme)
  (syntax lookup)
  (micalog core inliner)
  (prefix (micalog core keywords) %))

; (displayln
;   (fluent (empty-lookup)
;   (lookup+core)
;   (inline-expr #`(3 (%append (3 (%variable foo)) (4 (%take (4 6) 4)))))
;   (syntax->datum)))
