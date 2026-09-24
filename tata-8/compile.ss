(import
  (scheme)
  (lets)
  (code)
  (source-file-descriptor)
  (tata-8 expander)
  (tata-8 typed))

(lets
  ($in-path (car (command-line-arguments)))
  ($out-path (cadr (command-line-arguments)))
  ;($kt-path (string-append $name ".kt"))
  ($sfd (path->source-file-descriptor $in-path))
  ($annotation
    (call-with-input-file $in-path
      (lambda ($port)
        (lets
          ((values $annotation _) (get-datum/annotations $port $sfd 0))
          $annotation))))
  ($syntax (datum->syntax #'+ $annotation))
  ($code (expand-program expander $syntax))
  ($string (code-string $code))
  (call-with-output-file $out-path
    (lambda ($port)
      (put-string $port $string))
    '(replace)))
