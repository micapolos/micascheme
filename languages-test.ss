(import (micascheme) (check) (languages))

(define (test-language $id)
  (language
    (string-append "test" (symbol->string $id))
    (lambda ($port $sfd $bfp)
      (make-read
        (lambda ($port $sfd $bfp)
          (lets
            ((values $value/eof $bfp)
              (get-datum/annotations $port $sfd $bfp))
            (switch $value/eof
              ((eof? $eof)
                (values $eof $bfp))
              ((else $value)
                (values
                  (list (symbol->string (annotation-stripped $value)) $bfp)
                  $bfp)))))
         $port $sfd $bfp))
    (lambda ($line $environment)
      (switch (car $line)
        ((string-empty? _)
          '(void))
        ((else $string)
          (lets
            ($symbol (string->symbol $string))
            (list
              (cond
                ((top-level-bound? $symbol $environment) 'check-value)
                ((top-level-syntax? $symbol $environment) 'check-syntax)
                (else 'check-unbound))
              `',$symbol)))))))

(define source-file-descriptor-a (source-file-descriptor "foo.testa" 0))
(define source-file-descriptor-b (source-file-descriptor "foo.testb" 0))
(define source-file-descriptor-c (source-file-descriptor "foo.testc" 0))

(define test-language-a (test-language 'a))
(define test-language-b (test-language 'b))
(define test-languages (list test-language-a test-language-b (scheme-language "testss")))

; --- languages-extension-ref?

(check (equal? (languages-extension-ref? test-languages "testa") test-language-a))
(check (equal? (languages-extension-ref? test-languages "testb") test-language-b))
(check (equal? (languages-extension-ref? test-languages "testc") #f))

; --- languages-library-extensions

(check
  (equal?
    (languages-library-extensions test-languages)
    '((".testa" . ".so") (".testb" . ".so") (".testss" . ".so"))))

; --- languages-make-read

(lets
  ($read
    (languages-make-read
      test-languages
      (open-input-string "+ -")
      source-file-descriptor-a
      10
      (partial make-read get-datum/annotations)))
  (run
    (check (equal? ($read) '("testa" "+" 11)))
    (check (equal? ($read) '("testa" "-" 13)))
    (check (eof? ($read)))))

(lets
  ($read
    (languages-make-read
      test-languages
      (open-input-string "+ -")
      source-file-descriptor-b
      10
      (partial make-read get-datum/annotations)))
  (run
    (check (equal? ($read) '("testb" "+" 11)))
    (check (equal? ($read) '("testb" "-" 13)))
    (check (eof? ($read)))))

(lets
  ($read
    (languages-make-read
      test-languages
      (open-input-string "+ -")
      source-file-descriptor-c
      10
      (partial make-read get-datum/annotations)))
  (run
    (check
      (annotation=? ($read)
        (stripped-annotation '+ (make-source-object source-file-descriptor-c 10 11))))
    (check
      (annotation=? ($read)
        (stripped-annotation '- (make-source-object source-file-descriptor-c 12 13))))
    (check (eof? ($read)))))

; --- languages-datum->language?-datum

(check
  (equal?
    (values->list (languages-datum->language?-datum test-languages "foo"))
    `(#f "foo")))

(check
  (equal?
    (values->list (languages-datum->language?-datum test-languages '("testa" . "foo")))
    `(,test-language-a "foo")))

(check
  (equal?
    (values->list (languages-datum->language?-datum test-languages '("testb" . "foo")))
    `(,test-language-b "foo")))

(check
  (equal?
    (values->list (languages-datum->language?-datum test-languages '("testc" . "foo")))
    `(#f ("testc" . "foo"))))

; --- languages-expand

(check
  (equal?
    (languages-expand test-languages '("testa" "string-append" 123) (scheme-environment))
    '(check-value 'string-append)))

(check
  (equal?
    (languages-expand test-languages '("testb" "string-append" 123) (scheme-environment))
    '(check-value 'string-append)))

(check
  (equal?
    (languages-expand test-languages '("testc" "string-append" 123) (scheme-environment))
    `("testc" "string-append" 123)))

(check
  (equal?
    (languages-expand test-languages '"string-append" (scheme-environment))
    "string-append"))

; --- languages-make-library-extensions

(check
  (equal?
    (languages-make-library-extensions test-languages '((".ss" . ".so") (".java" . ".so")))
    '(
      (".testa" . ".so")
      (".testb" . ".so")
      (".testss" . ".so")
      (".ss" . ".so")
      (".java" . ".so"))))

; --- languages-make-read-handler

(lets
  ($read-handler
    (languages-make-read-handler test-languages default-make-read-handler))
  ($read
    ($read-handler
      (open-input-string "+ -")
      source-file-descriptor-a
      10))
  (run
    (check (equal? ($read) '("testa" "+" 11)))
    (check (equal? ($read) '("testa" "-" 13)))
    (check (eof? ($read)))))

(lets
  ($read-handler
    (languages-make-read-handler test-languages default-make-read-handler))
  ($read
    ($read-handler
      (open-input-string "+ -")
      source-file-descriptor-c
      10))
  (run
    (check
      (annotation=? ($read)
        (stripped-annotation '+ (make-source-object source-file-descriptor-c 10 11))))
    (check
      (annotation=? ($read)
        (stripped-annotation '- (make-source-object source-file-descriptor-c 12 13))))
    (check (eof? ($read)))))

; --- languages-make-expand

(check
  (equal?
    (app
      (languages-make-expand test-languages sc-expand)
      '("testa" "string-append" 123)
      (interaction-environment))
    '(check-value 'string-append)))

(check
  (equal?
    (app
      (languages-make-expand test-languages sc-expand)
      '("testa" "cond" 123)
      (interaction-environment))
    '(check-syntax 'cond)))

(check
  (equal?
    (app
      (languages-make-expand test-languages sc-expand)
      '("testc" "cond" 123)
      (interaction-environment))
    '("testc" "cond" 123)))

(check
  (equal?
    (app
      (languages-make-expand test-languages sc-expand)
      '("testss" "cond" 123)
      (interaction-environment))
    '("cond" 123)))

; --- languages-call

(check
  (works
    (languages-call test-languages
      (lambda ()
        (load "language-example.testa"
          (lambda ($datum)
            (check (equal? (car $datum) "testa"))
            (check (member (cadr $datum) (list "+" "-") ))
            (check (integer? (caddr $datum)))
            $datum))))))

(check
  (works
    (languages-call test-languages
      (lambda ()
        (load "language-example.testb"
          (lambda ($datum)
            (check (equal? (car $datum) "testb"))
            (check (member (cadr $datum) (list "fx+" "fx-")))
            (check (integer? (caddr $datum)))
            $datum))))))

(check
  (works
    (languages-call test-languages
      (lambda ()
        (load "language-example.testss")))))

; --- call-with-current-languages

(check
  (works
    (parameterize
      ((current-languages test-languages))
      (with-current-languages
        (load "language-example.testss")))))
