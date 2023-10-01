(import (micascheme) (tico term))

(with-generate-temporary-seed $tmp
  (check
    (equal?
      (term->datum `foo)
      `foo)))

(with-generate-temporary-seed $tmp
  (check
    (equal?
      (term->datum (function 1 (variable 0)))
      `(lambda ($tmp-0) $tmp-0))))

(with-generate-temporary-seed $tmp
  (check
    (equal?
      (term->datum (function 2 (variable 0)))
      `(lambda ($tmp-0 $tmp-1) $tmp-1))))

(with-generate-temporary-seed $tmp
  (check
    (equal?
      (term->datum (function 2 (variable 1)))
      `(lambda ($tmp-0 $tmp-1) $tmp-0))))

(with-generate-temporary-seed $tmp
  (check
    (equal?
      (term->datum (function 2 (application `string-append (list (variable 1) (variable 0)))))
      `(lambda ($tmp-0 $tmp-1) (string-append $tmp-0 $tmp-1)))))

(check
  (equal?
    (term->value
      (application
        (function 2
          (application `string-append (list (variable 1) (variable 0))))
        (list "foo" "bar")))
    "foobar"))
