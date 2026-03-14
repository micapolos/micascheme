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

(define test-language
  (language "test"
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

(check (language? test-language))
(check (not (language? "language")))

(check (string=? (language-extension test-language) "test"))

(check
  (equal?
    (language-library-extension test-language)
    '(".test" . ".so")))

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
