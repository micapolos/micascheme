(import
  (tt lang)
  (tt number)
  (tt boolean)
  (tt datum)
  (tt string)
  (tt list)
  (prefix (scheme) %))

(define-record
  (point
    (x number)
    (y number)
    (=
      (lambda ((p1 point) (p2 point))
        (and
          (= (point-x p1) (point-x p2))
          (= (point-y p1) (point-y p2)))))
    (datum
      (lambda ((p point))
        (cons 'point
          (cons (number->datum (point-x p))
            (cons (number->datum (point-y p))
              '())))))))

(check
  (datum=?
    (datum (point 10 20))
    '(point 10 20)))

(check
  (true?
    (=
      (point 10 10)
      (point 10 10))))

(check
  (false?
    (=
      (point 10 10)
      (point 10 20))))

; (print (point 10 20))
; (print (eq? (point 10 20) (point 10 20)))
; (print (->datum (point 10 20)))

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

(check
  (number=?
    (length (list 1 2 3))
    3))

(check
  (number=?
    (length (list))
    0))

(check
  (true?
    (list=? number=?
      (list 1 2 3)
      (link 1 (link 2 (link 3 null))))))

(check
  (boolean=?
    (list=? number=?
      (list 1 2 3)
      (list 1 2))
    #f))

(check
  (boolean=?
    (list=? number=?
      (list 1 2 3)
      (list 1 2 4))
    #f))
