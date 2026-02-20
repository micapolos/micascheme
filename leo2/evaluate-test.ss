(import
  (leo2 base)
  (leo2 term)
  (leo2 stdlib)
  (leo2 evaluate)
  (leo2 datum))

(check-evaluates
  (evaluated (string-term "foo"))
  (evaluated (string-term "foo")))

(check-evaluates
  (type 12)
  (evaluated (type 12)))

(check-evaluates
  (variable-term string-type 'x)
  (evaluated (variable-term string-type 'x)))

(check-evaluates
  (string-term "foo")
  (evaluated (string-term "foo")))

(check-evaluates
  (native-application-term
    string-type
    string-append
    (string-term "foo")
    (string-term "bar"))
  (evaluated (string-term "foobar")))

(check-evaluates
  (native-application-term
    string-type
    string-append
    (string-term "foo")
    (variable-term string-type 'x))
  (evaluated
    (native-application-term
      string-type
      string-append
      (evaluated (string-term "foo"))
      (evaluated (variable-term string-type 'x)))))

(check-evaluates
  (abstraction-term string-type
    (lambda ($x) $x))
  (evaluated
    (abstraction-term string-type
      (lambda ($x) (evaluated $x)))))

(check-evaluates
  (application-term
    (abstraction-term string-type
      (lambda ($x) $x))
    (string-term "foo"))
  (evaluated (string-term "foo")))

(check-evaluates
  (abstraction-type-term
    (type 0)
    (lambda ($x) $x))
  (evaluated
    (abstraction-type-term (evaluated (type 0))
      (lambda (v0) (evaluated v0)))))

(check-evaluates
  (application-term
    (application-term
      (abstraction-term string-type
        (lambda ($x)
          (abstraction-term string-type
            (lambda ($y)
              (native-application-term string-type string-append $x $y)))))
      (string-term "foo"))
    (string-term "bar"))
  (evaluated (string-term "foobar")))

(check-evaluates
  (branch-term
    (boolean-term #t)
    (string-term "true")
    (number-term 10))
  (evaluated (string-term "true")))

(check-evaluates
  (branch-term
    (boolean-term #f)
    (number-term 10)
    (string-term "false"))
  (evaluated (string-term "false")))

(check-evaluates
  (branch-term
    (variable-term boolean-type 'x)
    (string-term "foo")
    (string-term "foo"))
  (evaluated (string-term "foo")))

(check-evaluates
  (branch-term
    (variable-term boolean-type 'x)
    (string-term "foo")
    (string-term "bar"))
  (evaluated
    (typed
      (branch-type-term (variable-term boolean-type 'x) string-type string-type)
      (branch
        (evaluated (variable-term boolean-type 'x))
        (evaluated (string-term "foo"))
        (evaluated (string-term "bar"))))))

; (check-evaluates
;   (recursion
;     (lambda ($self)
;       (abstraction
;         (lambda ($n) $self))))
;   (evaluated
;     (recursion
;       (lambda (v0)
;         (evaluated
;           (abstraction
;             (lambda (v1)
;               (evaluated v0))))))))

; (check-evaluates
;   (recursion
;     (lambda ($self)
;       (abstraction
;         (lambda ($n) $n))))
;   (evaluated
;     (recursion
;       (lambda (v0)
;         (evaluated
;           (abstraction
;             (lambda (v1)
;               (evaluated v1))))))))

; (check-evaluates
;   (application
;     (recursion
;       (lambda ($self)
;         (abstraction
;           (lambda ($n)
;             (branch
;               (native-application (native zero?) (list $n))
;               (native "Done")
;               (application $self (native-application (native -) (list $n (native 1)))))))))
;     (native 1))
;   (evaluated (native "Done")))

; (check-evaluates
;   (application
;     (recursion
;       (lambda ($self)
;         (abstraction
;           (lambda ($n)
;             (branch
;               (native-application (native <) (list $n (native 2)))
;               $n
;               (native-application (native +)
;                 (list
;                   (application $self (native-application (native -) (list $n (native 1))))
;                   (application $self (native-application (native -) (list $n (native 2)))))))))))
;     (native 10))
;   (evaluated (native 55)))
