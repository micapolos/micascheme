(import
  (micascheme)
  (tico layout))

(check (equal? (layout-not-empty? (empty-layout)) #f))
(check (equal? (layout-not-empty? (simple-layout)) #t))
(check (equal? (layout-not-empty? (tuple-layout (list))) #t))
