(import
  (only (leo2 base) quote)
  (leo2 expand)
  (prefix (leo2 comptime) %)
  (prefix (leo2 term) %%))

(check
  (expand a-type)
  (expanded (%type 1) a-type))

(check
  (expand a-boolean)
  (expanded (%type 0) (variable a-boolean)))

(check
  (expand #t)
  (expanded (%native 'a-boolean) (native #t)))

(check
  (expand 123)
  (expanded (%native 'a-number) (native 123)))

(check
  (expand #\a)
  (expanded (%native 'a-char) (native #\a)))

(check
  (expand "foo")
  (expanded (%native 'a-string) (native "foo")))

(check
  (expand (native a-string (string-append "foo" "bar")))
  (expanded
    (%variable a-string)
    (native (string-append "foo" "bar"))))

(check
  (expand (native-apply a-number %string-length "foo"))
  (expanded
    (%variable a-number)
    (native-application
      (native %string-length)
      (list (native "foo")))))

(check
  (expand (a-lambda a-number a-string))
  (expanded
    (%type 0)
    (a-lambda
      (v0 (variable a-number))
      (variable a-string))))

(check
  (expand (a-lambda a-number a-boolean a-string))
  (expanded
    (%type 0)
    (a-lambda
      (v0 (variable a-number))
      (a-lambda
        (v1 (variable a-boolean))
        (variable a-string)))))

(check
  (expand (a-lambda (x : a-number) x))
  (expanded
    (%type 0)
    (a-lambda
      (v0 (variable a-number))
      (variable v0))))
