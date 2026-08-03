(import
  (tt lang)
  (tt list)
  (tt number)
  (tt string)
  (tt boolean)
  (tt datum))

(check (true? (list=? number=? (list) null)))

(check
  (true?
    (list=? number=?
      (list 1 2 3)
      (link 1 (link 2 (link 3 null))))))

(check (number=? (length (list 10 20 30)) 3))

(check
  (string=?
    (unlink
      (list 1 2 3)
      (lambda () "empty")
      (lambda ((n number) (l (list number))) "not empty"))
    "not empty"))

(check
  (true?
    (list=? string=?
      (map number->string (list 1 2 3))
      (list "1" "2" "3"))))

(check
  (datum=?
    (list->datum string->datum
      (list "1" "2" "3"))
    '("1" "2" "3")))

(check
  (string=?
    (fold string+ "0" (list "1" "2" "3"))
    "0123"))
