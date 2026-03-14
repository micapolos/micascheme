(import (scheme) (check) (lets) (procedure) (annotation) (eof) (read))

(check
  (equal?
    (syntax->datum (load-syntax-list #'+ "read-test-input.ss"))
    '((foo) (bar))))

(lets
  ($sfd (source-file-descriptor "test.txt" 123))
  ($read
    (make-read get-datum/annotations
      (open-input-string "10 20 30")
      $sfd
      10))
  (run
    (check
      (annotation=?
        ($read)
        (make-annotation 10 (make-source-object $sfd 10 12) 10)))
    (check
      (annotation=?
        ($read)
        (make-annotation 20 (make-source-object $sfd 13 15) 20)))
    (check
      (annotation=?
        ($read)
        (make-annotation 30 (make-source-object $sfd 16 18) 30)))
    (check (eof? ($read)))))
