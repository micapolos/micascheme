(import
  (only (leo2 base) quote)
  (leo2 expand)
  (prefix (leo2 comptime) %)
  (prefix (leo2 term) %%)
  (leo2 stdlib))

(check
  (expand type)
  (expanded
    (type-term 1)
    type))

(check
  (expand boolean)
  (expanded
    (symbol-type-term 'boolean)
    (symbol boolean)))

(check
  (expand number)
  (expanded
    (symbol-type-term 'number)
    (symbol number)))

(check
  (expand char)
  (expanded
    (symbol-type-term 'char)
    (symbol char)))

(check
  (expand string)
  (expanded
    (symbol-type-term 'string)
    (symbol string)))

(check
  (expand #t)
  (expanded boolean-type (literal #t)))

(check
  (expand 123)
  (expanded number-type (literal 123)))

(check
  (expand #\a)
  (expanded char-type (literal #\a)))

(check
  (expand "foo")
  (expanded string-type (literal "foo")))

(check
  (expand ("foo" $3))
  (expanded
    (indexed-type-term 2 string-type)
    (indexed 2 (literal "foo"))))

; (check
;   (expand (native a-string (string-append "foo" "bar")))
;   (expanded
;     string-type
;     (native (native (type 0) 'a-string) (string-append "foo" "bar"))))

; (check
;   (expand (native-apply a-number %string-length "foo"))
;   (expanded
;     number-type
;     (native-apply
;       (native (type 0) (quote a-number))
;       %string-length
;       (native (native (type 0) 'a-string) "foo"))))

; (check
;   (expand (a-lambda a-number a-string))
;   (expanded
;     (%type 0)
;     (a-lambda
;       (v0 (native (type 0) 'a-number))
;       (native (type 0) 'a-string))))

; (check
;   (expand (a-lambda a-number a-boolean a-string))
;   (expanded
;     (%type 0)
;     (a-lambda
;       (v0 (native (type 0) 'a-number))
;       (a-lambda
;         (v1 (native (type 0) 'a-boolean))
;         (native (type 0) 'a-string)))))

; (check
;   (expand (a-lambda (x : a-number) x))
;   (expanded
;     (%type 0)
;     (a-lambda
;       (v0 (native (type 0) 'a-number))
;       (variable (native (type 0) 'a-number) v0))))

(check
  (expand (lambda (x : number) "foo"))
  (expanded
    (%a-lambda (x number-type) string-type)
    (lambda
      (v0 (symbol number))
      (literal "foo"))))

; (check
;   (expand (lambda (x : number) x))
;   (expanded
;     (%a-lambda (x number-type) number-type)
;     (lambda
;       (v0 (symbol number))
;       (variable (symbol number) v0))))

; (check
;   (expand
;     (lambda (x : a-number)
;       (native-apply a-boolean %zero? x)))
;   (expanded
;     (%a-lambda (v0 number-type) boolean-type)
;     (lambda
;       (v0 (native (type 0) 'a-number))
;       (native-apply
;         (native (type 0) 'a-boolean)
;         %zero?
;         (variable (native (type 0) 'a-number) v0)))))

; (check
;   (expand
;     (lambda
;       (x : a-number)
;       (y : a-number)
;       (native-apply a-boolean %< x y)))
;   (expanded
;     (%a-lambda (v0 number-type)
;       (%a-lambda (v1 number-type)
;         boolean-type))
;     (lambda
;       (v0 (native (type 0) 'a-number))
;       (lambda (v1 (native (type 0) 'a-number))
;         (native-apply
;           (native (type 0) 'a-boolean)
;           %<
;           (variable (native (type 0) 'a-number) v0)
;           (variable (native (type 0) 'a-number) v1))))))

; (check
;   (expand (native-lambda %< a-number a-number a-boolean))
;   (expanded
;     (%a-lambda (v0 number-type)
;       (%a-lambda (v1 number-type)
;         boolean-type))
;     (lambda
;       (v0 (native (type 0) 'a-number))
;       (lambda (v1 (native (type 0) 'a-number))
;         (native-apply
;           (native (type 0) 'a-boolean)
;           %<
;           (variable (native (type 0) 'a-number) v0)
;           (variable (native (type 0) 'a-number) v1))))))



