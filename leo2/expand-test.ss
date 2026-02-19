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
