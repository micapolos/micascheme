(import (scheme) (check) (quote))

(check
  (equal?
    (quote-operator
      (string-append (string-append "a" "b") "c"))
    '(string-append "ab" "c")))

(check
  (equal?
    (quote-operator
      (
        (string-append "a" "b")
        (string-append "c" "d")
        (string-append "e" "f")))
    '(
      (string-append "a" "b")
      "cd"
      "ef")))

(check
  (equal?
    (quote-operator (string-append "a" "b"))
    '(string-append "a" "b")))
