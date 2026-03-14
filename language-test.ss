(import
  (scheme)
  (check)
  (lets)
  (read)
  (switch)
  (eof)
  (procedure)
  (string)
  (language))

(define (make-test-language $extension . $extensions)
  (language
    (cons $extension $extensions)
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
      (lets
        ($symbol (string->symbol $line))
        (list
          (cond
            ((top-level-bound? $symbol $environment) 'value)
            ((top-level-syntax? $symbol $environment) 'syntax)
            (else 'missing))
          $symbol)))))

(define test-language (make-test-language "testa" "testb"))
(define list-language (language-append test-language scheme-language))

(check (language? test-language))
(check (not (language? "language")))

(check
  (equal?
    (language-extensions test-language)
    (list "testa" "testb")))

(check
  (equal?
    (language-library-extensions test-language)
    '(
      (".testa" . ".so")
      (".testb" . ".so"))))

(lets
  ($read
    (language-make-read test-language
      (open-input-string (lines-string "foo" "bar"))
      (source-file-descriptor "source.test" 0)
      10))
  (run
    (check (equal? ($read) '("foo" 13)))
    (check (equal? ($read) '("bar" 17)))
    (check (eof? ($read)))))

(check
  (equal?
    (language-expand test-language "string-append" (scheme-environment))
    `(value string-append)))

(check
  (equal?
    (language-expand test-language "if" (scheme-environment))
    `(syntax if)))

(check
  (equal?
    (language-expand test-language "foo" (scheme-environment))
    `(missing foo)))

(check
  (works
    (language-call scheme-language
      (lambda ()
        (load "language-example.ss")))))

(check
  (works
    (with-language scheme-language
      (load "language-example.ss"))))
