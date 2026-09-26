(import
  (scheme)
  (lets)
  (code)
  (source-file-descriptor)
  (tata-8 expander)
  (tata-8 typed)
  (only (mica reader) read-file)
  (leo3 reader line)
  (system))

(lets
  ($in-path (car (command-line-arguments)))
  ($out-path (cadr (command-line-arguments)))
  ($sfd (path->source-file-descriptor $in-path))
  ($annotation (read-file line-annotation $in-path))
  ($syntax (datum->syntax #'+ $annotation))
  ($code (expand-program expander $syntax))
  ($string (code-string $code))
  (call-with-output-file $out-path
    (lambda ($port)
      (put-string $port $string))
    '(replace)))
