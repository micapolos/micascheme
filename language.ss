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
    language-expand)
  (import (scheme) (lets) (switch) (eof) (data))

  (data (language extension make-read-procedure expand-procedure))

  (define scheme-language (language "ss" default-make-read-handler sc-expand))

  (define (extension-language=? $extension $language)
    (string=? $extension (language-extension $language)))

  (define (language-make-read $language $port $sfd $bfp)
    ((language-make-read-procedure $language) $port $sfd $bfp))

  (define language-expand
    (case-lambda
      (($language $datum $environment)
        ((language-expand-procedure $language) $datum $environment))
      (($language $datum)
        (language-expand $datum (interaction-environment)))))

  (define (language-library-extension $language)
    `(
      ,(string-append "." (language-extension $language))
      .
      ".so"))
)
