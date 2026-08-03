(import
  (tt lang)
  (tt list)
  (tt number)
  (tt string)
  (tt boolean)
  (tt datum))

(check (true? (list=? = (list) null)))

(check
  (true?
    (list=? =
      (list 1 2 3)
      (link 1 (link 2 (link 3 null))))))

(check (= (length (list 10 20 30)) 3))

(check
  (string=?
    (unlink
      (list 1 2 3)
      (lambda () "empty")
      (lambda ((n number) (l (list number))) "not empty"))
    "not empty"))

(check
  (true?
    (list=? =
      (link 1 (link 2 (link 3 null)))
      (push (push (push null 3) 2) 1))))

(check
  (true?
    (list=? string=?
      (map number->string (list 1 2 3))
      (list "1" "2" "3"))))

(check
  (string=?
    (fold string+ "0" (list "1" "2" "3"))
    "0123"))

(check
  (true?
    (list=? =
      (intercalate (list 1 2 3) 0)
      (list 1 0 2 0 3))))

(check
  (true?
    (list=? =
      (reverse (list 1 2 3))
      (list 3 2 1))))
