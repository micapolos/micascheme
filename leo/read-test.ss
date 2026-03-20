(import
  (micascheme)
  (leo read))

(lets
  ($sfd (source-file-descriptor "foo" 123))
  ($read
    (make-leo-read
      (open-input-string "123\nfoo\n")
      $sfd
      0))
  (run
    (check
      (annotation=? ($read)
        (stripped-annotation 123 (make-source-object $sfd 0 3))))
    (check
      (annotation=? ($read)
        (stripped-annotation 'foo (make-source-object $sfd 4 7))))
    (check (eof? ($read)))))
