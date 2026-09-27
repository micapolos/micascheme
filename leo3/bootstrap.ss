(import
  (scheme)
  (lets)
  (switch)
  (eof)
  (procedure)
  (annotation)
  (leo3 path))

(import $system)

(define (make-leo-read $port $sfd $bfp)
  (lets
    ($fp (or $bfp 0))
      (lambda ()
        (lets
          ((values $datum $new-fp)
            (switch (get-line $port)
              ((eof? $eof)
                (values $eof $fp))
              ((else $line)
                (values
                  (stripped-annotation
                    `(begin
                      (display ,$line)
                      (newline))
                    (make-source-object $sfd $fp (+ $fp (string-length $line))))
                  (+ $fp (string-length $line) 1)))))
          (run (set! $fp $new-fp))
          $datum))))

(lets
  ($orig-make-read ($top-level-value '$make-read))
  ($set-top-level-value! '$make-read
    (lambda ($port $sfd $bfp)
      (
        (if
          (and
            (source-file-descriptor? $sfd)
            (leo-path? (source-file-descriptor-path $sfd) "leo"))
          make-leo-read
          $orig-make-read)
        $port $sfd $bfp))))

(library-extensions (cons '(".leo" . ".so") (library-extensions)))

(lets
  ($read (make-leo-read (open-input-string "foo\nbar\n") (source-file-descriptor "foo.txt" 0) 0))
  (run
    (pretty-print ($read))
    (pretty-print ($read))
    (pretty-print ($read))))

(include "tata/game.leo")
