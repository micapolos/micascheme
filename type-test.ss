(import 
  (micascheme) 
  (type))

(writeln
  (term->datum
    (application
      `string-append
      (list "foo" "bar"))))
