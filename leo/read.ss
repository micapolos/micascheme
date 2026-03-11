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
    (leo getter)
    ; Force loading (leo lang), so it's available when reading leo files.
    ; TODO: Find a workaround around that.
    (only (leo lang)))

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

  (define-rule-syntax (with-leo-read body ...)
    (parameterize
      (
        (optimize-level 3)
        (make-read-handler make-leo-read)
        (library-extensions '((".leo" . ".so"))))
      body
      ...))

  (define (load-leo-program $path)
    (with-leo-read (load-program $path)))
)
