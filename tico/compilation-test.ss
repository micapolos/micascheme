(import
  (micascheme)
  (tico compilation)
  (tico constant)
  (tico variable)
  (tico dependency)
  (tico packet)
  (tico datum)
  (tico evaluation)
  (tico variable))

(check
  (equal?
    (datum->compilation '(string-append "foo" "bar"))
    (compilation
      '(string-append "foo" "bar")
      (datum->constant '(string-append "foo" "bar")))))

(check
  (equal?
    (literal->compilation "foo")
    (compilation "foo" (constant "foo"))))

(check
  (equal?
    (compilation-top-level-datum
      (datum->compilation '(+ 1 2)))
    '(+ 1 2)))

(check
  (equal?
    (compilation-top-level-datum
      (compilation
        '(+ a b)
        (variable 1
          (stack
            (dependency 'a (literal->packet 1))
            (dependency 'b (literal->packet 2))))))
    (lets-datum
      (map dependency-lets-datum
        (list
          (dependency 'a (literal->packet 1))
          (dependency 'b (literal->packet 2))))
      '(+ a b))))

; --- compilation->generate-dependency-opt

(check
  (equal?
    (compilation->generate-dependency-opt
      (compilation 'foo
        (variable 3 (stack))))
    #f))

(check
  (equal?
    (with-generate-temporary-seed $tmp
      (compilation->generate-dependency-opt
        (compilation 'foo (constant "foo"))))
    (dependency '$tmp-0 (packet 'foo "foo"))))

; --- compilation-application

(check
  (equal?
    (compilation-application
      (datum->compilation 'string-append)
      (list
        (literal->compilation "foo")
        (literal->compilation "bar")))
    (datum->compilation
      '(string-append "foo" "bar"))))

(check
  (equal?
    (with-generate-temporary-seed $tmp
      (compilation-application
        (datum->compilation 'string-append)
        (list
          (compilation 'foo (variable 1 (stack (test-dependency d1))))
          (literal->compilation "bar"))))
    (compilation
      '(string-append foo "bar")
      (variable 1
        (stack
          (dependency '$tmp-0 (datum->packet 'string-append))
          (dependency '$tmp-1 (datum->packet "bar"))
          (test-dependency d1))))))

; --- compilation-abstraction

(check
  (equal?
    (compilation-abstraction
      (list 'v1 'v2)
      (compilation '(string-append v1 v2)
        (variable 1
          (stack
            (dependency 'v1 (packet "foo" "foo"))
            (dependency 'v2 (packet "bar" "bar"))))))
    (compilation
      (datum-abstraction
        (list 'v1 'v2)
        '(string-append v1 v2))
      (evaluation-abstraction
        2
        (variable 1
          (stack
            (dependency 'v1 (packet "foo" "foo"))
            (dependency 'v2 (packet "bar" "bar"))))
        (lambda () '(string-append v1 v2))))))

; --- compilation-struct

(check
  (equal?
    (compilation-struct 'x
      (list
        (compilation "foo" (constant "foo"))
        (compilation "bar" (constant "bar"))))
    (compilation
      (datum-struct 'x (list "foo" "bar"))
      (constant-struct 'x (list (constant "foo") (constant "bar"))))))

(check
  (equal?
    (with-generate-temporary-seed $tmp
      (compilation-struct 'x
        (list
          (compilation 'foo (test-variable 1 d1 d2))
          (compilation '(identity "foo") (constant "foo"))
          (compilation 'bar (test-variable 2 d3 d4))
          (compilation '(identity "bar") (constant "bar")))))
    (compilation
      (datum-struct 'x
        (list 'foo '$tmp-0 'bar '$tmp-1))
      (variable
        (variable-index-flatten (list 1 2))
        (stack
          (test-dependency d1)
          (test-dependency d2)
          (dependency '$tmp-0 (packet '(identity "foo") "foo"))
          (test-dependency d3)
          (test-dependency d4)
          (dependency '$tmp-1 (packet '(identity "bar") "bar")))))))
