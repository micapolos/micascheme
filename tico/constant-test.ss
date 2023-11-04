(import
  (micascheme)
  (tico constant)
  (tico datum))

(check
  (equal?
    (datum->constant '(string-append "foo" "bar"))
    (constant (datum->value '(string-append "foo" "bar")))))
