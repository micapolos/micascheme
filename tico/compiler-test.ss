(import
  (micascheme)
  (tico compiler)
  (tico compiled))

(check
  (equal?
    (compiler-compiled (literal-compiler "foo"))
    (literal->compiled "foo")))

(check
  (equal?
    (with-generate-temporary-seed $tmp
      (compiler-compiled
        (compiler-globalize
          (literal-compiler "foo"))))
    (compiled
      (globals (symbolic '$tmp-0 (packet "foo" "foo")))
      (typed (string-type) (packet '$tmp-0 (constant "foo"))))))
