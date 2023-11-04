(import
  (micascheme)
  (tico compiler)
  (tico compiled))

(check
  (equal?
    (compiler-datum (native-compiler (string-type) "foo"))
    "foo"))

(check
  (equal?
    (compiler-datum (literal-compiler "foo"))
    "foo"))

(check
  (equal?
    (with-generate-temporary-seed $tmp
      (compiler-datum
        (compiler-globalize
          (literal-compiler "foo"))))
    `(lets
      ($tmp-0 "foo")
      $tmp-0)))

; --- application

; (check
;   (equal?
;     (with-generate-temporary-seed $tmp
;       (compiler-datum
;         (application-compiler
;           (compiler-globalize
;             (compiler
;               (typed
;                 (arrow (list (string-type) (string-type)) (string-type))
;                 (packet 'string-append (constant string-append)))))
;           (list
;             (compiler-globalize
;               (compiler
;                 (typed
;                   (string-type)
;                   (packet "foo" (constant "foo")))))
;             (compiler-globalize
;               (compiler
;                 (typed
;                   (string-type)
;                   (packet "bar" (constant "bar")))))))))
;     '(string-append "foo" "bar")))
