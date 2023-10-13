(import (micascheme) (tico term))

; term->datum

(with-generate-temporary-seed $tmp
  (check
    (equal?
      (term->datum (native 128))
      128)))

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
      (term->datum
        (function 2
          (application
            (native `string-append)
            (list (variable 1) (variable 0)))))
      `(lambda ($tmp-0 $tmp-1)
        (string-append $tmp-0 $tmp-1)))))

; term->value

(check
  (equal?
    (term->value
      (application
        (function 2
          (application
            (native `string-append)
            (list (variable 1) (variable 0))))
        (list (native "foo") (native "bar"))))
    "foobar"))

; term->free-variable-count

(check
  (equal?
    (term->free-variable-count (native `foo))
    0))

(check
  (equal?
    (term->free-variable-count (variable 3))
    4))

(check
  (equal?
    (term->free-variable-count (function 2 (variable 3)))
    2))

(check
  (equal?
    (term->free-variable-count (function 2 (application (variable 3) (list (variable 5)))))
    4))
