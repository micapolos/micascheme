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
    (check (eof? ($read)))

    (check
      (annotation=?
        (car (values->list (leo-read-annotation (open-input-string "123\n") $sfd 0)))
        (stripped-annotation 123 (make-source-object $sfd 0 3))))

    (check
      (equal?
        (car (values->list (leo-read-annotation (open-input-string "") $sfd 0)))
        eof))

    (check
      (equal?
        (leo-read (open-input-string "123\n"))
        123))

    (check
      (equal?
        (leo-read (open-input-string ""))
        eof))))
