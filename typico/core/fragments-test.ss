(import
  (typico base)
  (typico fragment)
  (prefix (typico core fragments) %))

(check (equal? %string-append (fragment '((scheme)) 'string-append)))
