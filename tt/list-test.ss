(import (tt lang) (tt list) (tt number) (tt string) (tt boolean))

(check (true? (list=? number=? (make-list) null)))

(check
  (true?
    (list=? number=?
      (make-list 1 2 3)
      (link 1 (link 2 (link 3 null))))))

(check (number=? (length (make-list 10 20 30)) 3))

(check
  (string=?
    (unlink
      (make-list 1 2 3)
      (lambda () "empty")
      (lambda ((n number) (l (list number))) "not empty"))
    "not empty"))

(check
  (true?
    (list=? string=?
      (map number->string (make-list 1 2 3))
      (make-list "1" "2" "3"))))
