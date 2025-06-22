(library (read)
  (export
    path-source-file-descriptor
    load-syntax-list)
  (import
    (scheme)
    (lets)
    (switch)
    (stack))

  (define (push-read-syntax $stack $port $sfd $pos $id)
    (lets
      ((values $obj $pos) (get-datum/annotations $port $sfd $pos))
      (switch $obj
        ((eof-object? _) $stack)
        ((else $syntax)
          (push-read-syntax (push $stack (datum->syntax $id $syntax)) $port $sfd $pos $id)))))

  (define (path-source-file-descriptor $path)
    (lets
      ($binary-port (open-file-input-port $path))
      (dynamic-wind
        (lambda () #f)
        (lambda () (make-source-file-descriptor $path $binary-port))
        (lambda () (close-port $binary-port)))))

  (define (load-syntax-list $id $path)
    (call-with-input-file $path
      (lambda ($port)
        (reverse
          (push-read-syntax
            (stack)
            $port
            (path-source-file-descriptor $path)
            0
            $id)))))
)
