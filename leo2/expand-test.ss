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

; (check
;   (expand (native-lambda %< a-number a-number a-boolean))
;   (expanded 1 2))

(check
  (expand (native-apply a-number %string-length "foo"))
  (expanded
    (%variable a-number)
    (native-apply
      (native %string-length)
      (native "foo"))))

(check
  (expand (lambda (x : a-number) "foo"))
  (expanded
    (%a-lambda (v0 (%variable a-number)) (%native 'a-string))  ; TODO: is (%native a-string) correct?
    (lambda v0 (native "foo"))))

(check
  (expand (lambda (x : a-number) x))
  (expanded
    (%a-lambda (v0 (%variable a-number)) (%variable a-number))
    (lambda v0 (variable v0))))

(check
  (expand
    (lambda (x : a-number)
      (native-apply a-boolean %zero? x)))
  (expanded
    (%a-lambda
      (v0 (%variable a-number))
      (%variable a-boolean))
    (lambda v0
      (native-apply
        (native %zero?)
        (variable v0)))))

(check
  (expand
    (lambda
      (x : a-number)
      (y : a-number)
      (native-apply a-boolean %< x y)))
  (expanded
    (%a-lambda
      (v0 (%variable a-number))
      (%a-lambda
        (v1 (%variable a-number))
        (%variable a-boolean)))
    (lambda v0
      (lambda v1
        (native-apply
          (native %<)
          (variable v0)
          (variable v1))))))

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
