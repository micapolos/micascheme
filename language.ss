(library (language)
  (export
    language
    language?
    language-extension
    language-make-read-procedure
    language-expand-procedure

    scheme-language
    extension-language=?

    language-library-extension
    language-make-read
    language-expand

    language-call
    with-language
    define-language)
  (import
    (scheme)
    (lets)
    (switch)
    (eof)
    (data)
    (syntax))

  (data (language extension make-read-procedure expand-procedure))

  (define (scheme-language $extension)
    (language $extension default-make-read-handler sc-expand))

  (define (extension-language=? $extension $language)
    (string=? $extension (language-extension $language)))

  (define (language-make-read $language $port $sfd $bfp)
    ((language-make-read-procedure $language) $port $sfd $bfp))

  (define (language-expand $language $datum $environment)
    ((language-expand-procedure $language) $datum $environment))

  (define (language-library-extension $language)
    `(
      ,(string-append "." (language-extension $language))
      .
      ".so"))

  (define (language-call $language $procedure)
    (parameterize
      (
        (library-extensions (list (language-library-extension $language)))
        (make-read-handler (language-make-read-procedure $language))
        (current-expand (language-expand-procedure $language)))
      ($procedure)))

  (define-rule-syntax (with-language language x xs ...)
    (language-call language
      (lambda () x xs ...)))

  (define-rule-syntax (define-language id language)
    (define-rule-syntax (id x xs (... ...))
      (with-language language x xs (... ...))))
)
