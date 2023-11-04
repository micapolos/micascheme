(import
  (micascheme)
  (tico type)
  (tico term)
  (tico compilation))

(check
  (equal?
    (literal->term "foo")
    (term
      (literal-type "foo")
      (literal->compilation "foo"))))

(check
  (equal?
    (type-datum->term (string-type) "foo")
    (term (string-type) (datum->compilation "foo"))))
