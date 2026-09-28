(import
  (scheme)
  (lets)
  (code)
  (source-file-descriptor)
  (tata expander)
  (tata typed)
  (leo3 read)
  (leo exception-handler)
  (system))

(with-exception-handler
  leo-exception-handler
  (lambda ()
    (lets
      ($in-path (car (command-line-arguments)))
      ($out-path (cadr (command-line-arguments)))
      ($sfd (path->source-file-descriptor $in-path))
      ($annotation (car (read-file $in-path)))
      ($syntax (datum->syntax #'+ $annotation))
      ($code (expand-program expander $syntax))
      ($string (code-string $code))
      (call-with-output-file $out-path
        (lambda ($port)
          (put-string $port $string))
        '(replace)))))
