(library (leo read)
  (export
    leo-read
    make-leo-read
    with-leo-read
    leo-path
    load-leo-program)
  (import
    (micascheme)
    (getter)
    (leo getter))

  (define (leo-read $port $sfd? $ann? $bfp?)
    (lets
      ((values $value $bfp $line $column)
        (getter-get!
          (or-eof-getter (if $ann? line-annotation-getter line-getter))
          $port
          ; TODO: Make it work without it.
          (or $sfd? (source-file-descriptor "noname" 0))
          0 ; indent
          (or $bfp? 0)
          0 ; line
          0 ; column
          ))
      (values $value $bfp)))

  (define (make-leo-read $port $sfd $bfp)
    (lets
      ($line 0)
      ($column 0)
      (lambda ()
        (lets
          ((values $value $new-bfp $new-line $new-column)
            (getter-get!
              (or-eof-getter line-annotation-getter)
              $port
              $sfd
              0 ; indent
              $bfp
              $line
              $column))
          (run
            (set! $bfp $new-bfp)
            (set! $line $new-line)
            (set! $column $new-column))
          $value))))

  (define (leo-path $components)
    (string-append
      (apply string-append (intercalate (cons "leo" $components) "/"))
      ".leo"))

  (define leo-path-extension "leo")

  (define (path-leo? $path)
    (string=? (path-extension $path) leo-path-extension))

  (define (source-file-descriptor-leo? $sfd)
    (path-leo? (source-file-descriptor-path $sfd)))

  (define-rule-syntax (with-leo-read body ...)
    (parameterize
      (
        (optimize-level 3)
        (make-read-handler
          (lambda ($port $sfd $bfp)
            (if (source-file-descriptor-leo? $sfd)
              (make-leo-read $port $sfd $bfp)
              (default-make-read-handler $port $sfd $bfp))))
        (library-extensions
          (cons
            '(".leo" . ".so")
            (library-extensions))))
      body
      ...))

  (define (load-leo-program $path)
    (with-leo-read (load-program $path)))
)
