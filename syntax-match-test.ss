(import (scheme) (check) (syntax) (generate) (syntax-match))

(check-equal? (syntax-match? #'1 1 "ok") "ok")
(check-equal? (syntax-match? #'1 2 "ok") #f)

(check-equal? (syntax-match? #'+ + (datum +)) '+)
(check-equal? (syntax-match? #'+ - (datum +)) #f)

(check-equal? (syntax-match? #'(+ +) (+ +) (datum +)) '+)
(check-equal? (syntax-match? #'(+ -) (+ -) (datum +)) '+)

(check-equal? (syntax-match? #'(+ 1 2) #'x (datum x)) '(+ 1 2))

(define-syntax-match? syntax-string
  (lambda ($pattern $body)
    (syntax-case $pattern ()
      ((_ s)
        (let (($tmp (generate-identifier #'s)))
          (values
            (list)
            $tmp
            (list #`(string? (datum #,$tmp)))
            #`(let ((s (datum #,$tmp))) #,$body)))))))

(check-equal?
  (syntax-match? #'"foo" (syntax-string s) (string-append s "!"))
  "foo!")
