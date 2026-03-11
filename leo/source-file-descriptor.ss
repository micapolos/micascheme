(library (leo source-file-descriptor)
  (export source-file-descriptor-leo?)
  (import
    (micascheme)
    (leo path))

  (define (source-file-descriptor-leo? $sfd)
    (path-leo? (source-file-descriptor-path $sfd)))
)
