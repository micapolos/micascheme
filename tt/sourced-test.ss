(import (scheme) (check) (tt sourced))

(define source-1 (make-source-object (source-file-descriptor "1.txt" 1) 10 100))
(define source-2 (make-source-object (source-file-descriptor "2.txt" 2) 20 200))

(check (sourced=? string=? "foo" "foo"))
(check (not (sourced=? string=? "foo" "bar")))

(check
  (sourced=? string=?
    (sourced source-1 "foo")
    (sourced source-1 "foo")))

(check
  (not
    (sourced=? string=?
      (sourced source-1 "foo")
      (sourced source-1 "bar"))))

(check
  (not
    (sourced=? string=?
      (sourced source-1 "foo")
      (sourced source-2 "foo"))))

(check
  (sourced=? string=?
    (sourced source-1
      (sourced source-2 "foo"))
    (sourced source-1
      (sourced source-2 "foo"))))


(check
  (sourced=? string=?
    (sourced-map
      (lambda ($rewrap $string)
        (string-append $string "!"))
      "foo")
    "foo!"))

(check
  (sourced=? string=?
    (sourced-map
      (lambda ($rewrap $string)
        (string-append $string "!"))
      (sourced
        (make-source-object (source-file-descriptor "foo.txt" 10) 20 30)
        "foo"))
    "foo!"))

(check
  (sourced=? string=?
    (sourced-map
      (lambda ($rewrap $string)
        ($rewrap (string-append $string "!")))
      (sourced
        (make-source-object (source-file-descriptor "foo.txt" 10) 20 30)
        "foo"))
    (sourced
      (make-source-object (source-file-descriptor "foo.txt" 10) 20 30)
      "foo!")))

(check
  (sourced=? string=?
    (sourced-map
      (lambda ($rewrap $string)
        ($rewrap (string-append $string "!")))
      (sourced
        (make-source-object (source-file-descriptor "foo.txt" 10) 20 30)
        (sourced
          (make-source-object (source-file-descriptor "bar.txt" 40) 50 60)
          "foo")))
    (sourced
      (make-source-object (source-file-descriptor "foo.txt" 10) 20 30)
      (sourced
        (make-source-object (source-file-descriptor "bar.txt" 40) 50 60)
        "foo!"))))
