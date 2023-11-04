(import
  (micascheme)
  (tico compilation)
  (tico constant))

(check
  (equal?
    (datum->compilation '(string-append "foo" "bar"))
    (compilation
      '(string-append "foo" "bar")
      (datum->constant '(string-append "foo" "bar")))))

(check
  (equal?
    (literal->compilation "foo")
    (compilation "foo" (constant "foo"))))
