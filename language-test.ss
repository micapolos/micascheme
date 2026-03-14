(import
  (scheme)
  (check)
  (lets)
  (read)
  (switch)
  (eof)
  (procedure)
  (string)
  (annotation)
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

; --- language?

(check (language? test-language))
(check (not (language? "language")))

; --- language-extensions

(check
  (equal?
    (language-extensions test-language)
    (list "testa" "testb")))

; --- language-library-extensions

(check
  (equal?
    (language-library-extensions test-language)
    '(
      (".testa" . ".so")
      (".testb" . ".so"))))

; --- language-make-read

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

; --- language-expand

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

; --- language-append

(lets
  ($language (language-append test-language scheme-language))
  (run
    ; --- language-extensions
    (check
      (equal?
        (language-extensions $language)
        (list "testa" "testb" "ss" "sls" "scm" "sch")))

    ; --- language-read .testa
    (lets
      ($sfd (source-file-descriptor "source.testa" 0))
      ($read
        (language-make-read $language
          (open-input-string (lines-string "foo" "bar"))
          $sfd
          10))
      (run
        (check
          (equal? ($read)
            (cons
              (language-expand-procedure test-language)
              '("foo" 13))))
        (check
          (equal? ($read)
            (cons
              (language-expand-procedure test-language)
              '("bar" 17))))
        (check (eof? ($read)))))

    ; --- language-read .ss
    (lets
      ($sfd (source-file-descriptor "source.ss" 0))
      ($read
        (language-make-read $language
          (open-input-string (lines-string "foo" "bar"))
          $sfd
          10))
      (run
        (check
          (datum/annotation=? ($read)
            (cons
              sc-expand
              (stripped-annotation
                'foo
                (make-source-object $sfd 10 13)))))
        (check
          (datum/annotation=? ($read)
            (cons
              sc-expand
              (stripped-annotation
                'bar
                (make-source-object $sfd 14 17)))))
        (check (eof? ($read)))))

    ; --- test-language expand
    (check
      (equal?
        (language-expand $language
          (cons
            (language-expand-procedure test-language)
            "string-append")
          (scheme-environment))
        `(value string-append)))

    ; --- scheme-language expand
    (check
      (equal?
        (language-expand $language
          (cons sc-expand '(+ 2 2))
          (scheme-environment))
        '(($primitive 2 +) 2 2)))))

; --- language-call

(check
  (works
    (language-call scheme-language
      (lambda ()
        (load "language-example.ss")))))

; --- with-language

(check
  (works
    (with-language scheme-language
      (load "language-example.ss"))))
