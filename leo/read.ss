(library (leo read)
  (export
    leo-read
    make-leo-read
    with-leo-read)
  (import
    (micascheme)
    (getter)
    (leo getter)
    (leo path)
    (leo expand)
    (leo source-file-descriptor))

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

  (define-rule-syntax (with-leo-read body ...)
    (lets
      ($make-read-handler (make-read-handler))
      ($current-expand (current-expand))
      ($library-extensions (library-extensions))
      (parameterize
        (
          (make-read-handler
            (lambda ($port $sfd $bfp)
              (if (source-file-descriptor-leo? $sfd)
                (make-leo-read $port $sfd $bfp)
                ($make-read-handler $port $sfd $bfp))))
          (current-expand
            (lambda ($form . $args)
              (apply $current-expand (leo-expand $form) $args)))
          (library-extensions
            (cons
              '(".leo" . ".so")
              $library-extensions)))
        body
        ...)))

  ; Invoke lang library not, so it's loaded and cached using scheme reader, and not leo reader.
  (invoke-library '(leo lang))
)
