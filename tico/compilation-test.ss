(import
  (micascheme)
  (tico compilation)
  (tico constant)
  (tico variable)
  (tico dependency)
  (tico packet))

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
    (compilation-value
      (datum->compilation '(+ 1 2)))
    3))

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
