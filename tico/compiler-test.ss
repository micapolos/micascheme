(import
  (micascheme)
  (tico compiled)
  (tico compiler))

(check
  (equal?
    (compiler-comptime (literal-compiler "foo"))
    "foo"))

(check
  (equal?
    (with-generate-temporary-seed $tmp
      (compiler-comptime
        (compiler-globalize
          (literal-compiler "foo"))))
    '(lets ($tmp-0 "foo") $tmp-0)))

(check
  (equal?
    (with-generate-temporary-seed $tmp
      (compiler-comptime
        (struct-compiler 'foo
          (list
            (literal-compiler #f)
            (compiler-globalize (literal-compiler 128))
            (compiler-globalize (literal-compiler "foo"))))))
    '(lets
      ($tmp-0 128)
      ($tmp-1 "foo")
      (vector #f $tmp-0 $tmp-1))))
