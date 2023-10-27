(import
  (micascheme)
  (tico type)
  (tico value)
  (tico expression)
  (tico parser))

(check
  (equal?
    (datum->compiled `boolean)
    (value-compiled (boolean-type))))

(check
  (equal?
    (datum->compiled `number)
    (value-compiled (number-type))))

(check
  (equal?
    (datum->compiled `string)
    (value-compiled (string-type))))

(check
  (equal?
    (datum->compiled `(scheme string (string-append "a" "b")))
    (compiled
      (string-type)
      (packet
        (expression `(string-append "a" "b") 0)
        (constant "ab")))))

(check
  (equal?
    (datum->compiled #f)
    (value-compiled #f)))

(check
  (equal?
    (datum->compiled 128)
    (value-compiled 128)))

(check
  (equal?
    (datum->compiled "foo")
    (value-compiled "foo")))

(check
  (equal?
    (datum->compiled
      `(struct vec
        (scheme number (+ 1 2))
        (scheme string (string-append "a" "b"))))
    (compiled
      (struct-type `vec (list (number-type) (string-type)))
      (packet
        (expression (tuple-expression (list `(+ 1 2) `(string-append "a" "b"))) 0)
        (constant (tuple-value (list 3 "ab")))))))

(check
  (raises?
    (lambda ()
      (datum->compiled `(struct "not-name" (scheme number n) (scheme string s))))))

(check
  (equal?
    (datum->compiled
      `(vec
        (scheme number (+ 1 2))
        (scheme string (string-append "a" "b"))))
    (compiled
      (struct-type `vec (list (number-type) (string-type)))
      (packet
        (expression (tuple-expression (list `(+ 1 2) `(string-append "a" "b"))) 0)
        (constant (tuple-value (list 3 "ab")))))))

(check
  (equal?
    (datum->compiled `(type (scheme number 1)))
    (value-compiled (number-type))))

(check
  (raises?
    (lambda ()
      (datum->compiled `()))))
