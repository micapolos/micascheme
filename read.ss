(library (read)
  (export
    load-syntax-list)
  (import
    (scheme)
    (lets)
    (switch)
    (stack))

  (define (push-read-syntax $stack $port $sfd $pos)
    (lets
      ((values $obj $pos) (get-datum/annotations $port $sfd $pos))
      (switch $obj
        ((eof-object? _) $stack)
        ((else $syntax)
          (push-read-syntax (push $stack $syntax) $port $sfd $pos)))))

  (define (load-syntax-list $path)
    (lets
      ($binary-port (open-file-input-port $path))
      ($sfd (make-source-file-descriptor $path $binary-port))
      (_ (close-port $binary-port))
      (call-with-input-file $path
        (lambda ($port)
          (reverse (push-read-syntax (stack) $port $sfd 0))))))
)
