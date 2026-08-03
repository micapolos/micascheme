(import
  (tt lang)
  (tt number)
  (tt boolean)
  (tt datum)
  (tt string)
  (tt list)
  (prefix (scheme) %))

(define-record (point (x number) (y number)))

(define (point=? (p1 point) (p2 point))
  (and
    (= (point-x p1) (point-x p2))
    (= (point-y p1) (point-y p2))))

(define (point->datum (p point))
  (cons 'point
    (cons (number->datum (point-x p))
      (cons (number->datum (point-y p))
        '()))))

(check
  (datum=?
    (point->datum (point 10 20))
    '(point 10 20)))

(check
  (point=?
    (point 10 10)
    (point 10 10)))

(check
  (not
    (point=?
      (point 10 10)
      (point 10 20))))

(print (point 10 20))
(print (point->datum (point 10 20)))

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
  (=
    (length (make-list 1 2 3))
    3))

(check
  (=
    (length (make-list))
    0))

(check
  (boolean=?
    (list=? =
      (make-list 1 2 3)
      (link 1 (link 2 (link 3 null))))
    #t))

(check
  (boolean=?
    (list=? =
      (make-list 1 2 3)
      (make-list 1 2))
    #f))

(check
  (boolean=?
    (list=? =
      (make-list 1 2 3)
      (make-list 1 2 4))
    #f))
