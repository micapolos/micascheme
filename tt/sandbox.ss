(import
  (tt lang)
  (tt number)
  (tt boolean)
  (tt datum)
  (tt string)
  (tt list)
  (prefix (scheme) %))

(define-record (point (x number) (y number)))

(check
  (point->datum (point 10 20))
  (typed datum (point 10 20)))

(check
  (point=?
    (point 10 10)
    (point 10 10))
  (typed boolean #t))

(check
  (point=?
    (point 10 10)
    (point 10 11))
  (typed boolean #f))

; (print (point 10 20))
; (print (point->datum (point 10 20)))

(check null
  (typed
    (forall $0 (list $0))
    ()))

(check
  (link 123 null)
  (typed
    (list number)
    (123)))

(check
  (link "foo" (link "bar" null))
  (typed
    (list string)
    ("foo" "bar")))

(check
  (string=?
    (unlink
      null
      (lambda () "")
      (lambda ((s string) (l (list string)))
        (string-append s (number->string (length l)))))
    ""))

(check
  (string=?
    (unlink
      (link "foo" (link "bar" (link "zoo" null)))
      (lambda () "")
      (lambda ((s string) (l (list string)))
        (string-append s (number->string (length l)))))
    "foo2"))
