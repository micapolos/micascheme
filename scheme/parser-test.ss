(import
  (micascheme)
  (parser)
  (scheme parser))

(check (equal? (parse (identifier-parser) "") (parse-error 1 1)))

; invalid initial
(check (equal? (parse (identifier-parser) "1") (parse-error 1 1)))
(check (equal? (parse (identifier-parser) "(") (parse-error 1 1)))
(check (equal? (parse (identifier-parser) ")") (parse-error 1 1)))
(check (equal? (parse (identifier-parser) "[") (parse-error 1 1)))
(check (equal? (parse (identifier-parser) "]") (parse-error 1 1)))
(check (equal? (parse (identifier-parser) "{") (parse-error 1 1)))
(check (equal? (parse (identifier-parser) "}") (parse-error 1 1)))
(check (equal? (parse (identifier-parser) ".") (parse-error 1 2)))
(check (equal? (parse (identifier-parser) "#") (parse-error 1 1)))
(check (equal? (parse (identifier-parser) "@") (parse-error 1 1)))
(check (equal? (parse (identifier-parser) ",") (parse-error 1 1)))
(check (equal? (parse (identifier-parser) "`") (parse-error 1 1)))
(check (equal? (parse (identifier-parser) "'") (parse-error 1 1)))
(check (equal? (parse (identifier-parser) "\"") (parse-error 1 1)))

; valid initial
(check (equal? (parse (identifier-parser) "a") 'a))
(check (equal? (parse (identifier-parser) "z") 'z))
(check (equal? (parse (identifier-parser) "!") '!))
(check (equal? (parse (identifier-parser) "$") '$))
(check (equal? (parse (identifier-parser) "%") '%))
(check (equal? (parse (identifier-parser) "&") '&))
(check (equal? (parse (identifier-parser) "*") '*))
(check (equal? (parse (identifier-parser) "/") '/))
(check (equal? (parse (identifier-parser) ":") ':))
(check (equal? (parse (identifier-parser) "<") '<))
(check (equal? (parse (identifier-parser) "=") '=))
(check (equal? (parse (identifier-parser) ">") '>))
(check (equal? (parse (identifier-parser) "?") '?))
(check (equal? (parse (identifier-parser) "~") '~))
(check (equal? (parse (identifier-parser) "_") '_))
(check (equal? (parse (identifier-parser) "^") '^))

; valid subsequent
(check (equal? (parse (identifier-parser) "aa") 'aa))
(check (equal? (parse (identifier-parser) "az") 'az))
(check (equal? (parse (identifier-parser) "a1") 'a1))
(check (equal? (parse (identifier-parser) "a+") 'a+))
(check (equal? (parse (identifier-parser) "a-") 'a-))
(check (equal? (parse (identifier-parser) "a.") 'a.))

; invalid subsequent
(check (equal? (parse (identifier-parser) "a(") (parse-error 1 2)))
(check (equal? (parse (identifier-parser) "a)") (parse-error 1 2)))
(check (equal? (parse (identifier-parser) "a[") (parse-error 1 2)))
(check (equal? (parse (identifier-parser) "a]") (parse-error 1 2)))
(check (equal? (parse (identifier-parser) "a{") (parse-error 1 2)))
(check (equal? (parse (identifier-parser) "a}") (parse-error 1 2)))
(check (equal? (parse (identifier-parser) "a#") (parse-error 1 2)))

; peculiar identifiers
(check (equal? (parse (identifier-parser) "+") '+))
(check (equal? (parse (identifier-parser) "-") '-))
(check (equal? (parse (identifier-parser) "...") '...))
