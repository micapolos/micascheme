(import
  (tt lang)
  (prefix (scheme) %)
  (prefix (check) %)
  (prefix (tt hoas) %)
  (prefix (tt hoas-compiler) %)
  (prefix (tt primitive) %))

(define-type point)
(define-type (list 1))
(define-type (pair 2))

(define my-point (typed point #f))

(%check
  (%equal?
    (tt (pi number string boolean))
    (%arrow (%list %number-type %string-type) %boolean-type)))

(define my-string "foo")
(define my-number 10)
(define my-boolean #t)
(define my-char #\a)

(define + (lambda (x number) (y number) x))
