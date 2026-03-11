(library (leo leo)
  (export with-leo)
  (import
    (micascheme)
    (leo expand)
    (leo read)
    (leo source-file-descriptor))

  (define-rule-syntax (with-leo body ...)
    (lets
      ($make-read-handler (make-read-handler))
      ($current-expand (current-expand))
      ($library-extensions (library-extensions))
      (run
        (invoke-library '(leo lang))
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
          body ...))))
)
