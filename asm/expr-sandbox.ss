(import
  (only (micascheme) pretty-print syntax->datum syntax syntax-eval force expand quote)
  (asm expr-lang))

(pretty-print (syntax->datum (typed #'1)))
(pretty-print (syntax->datum (typed #'"foo")))
(pretty-print (syntax->datum (typed #'#f)))
(pretty-print (syntax->datum (typed #'(string (string-append "foo" "bar")))))

(pretty-print (syntax->datum (expr-of #'number (typed #'1))))
(pretty-print (syntax->datum (expr-of #'string (typed #'"foo"))))
(pretty-print (syntax->datum (expr-of #'boolean (typed #'#t))))

(pretty-print (syntax->datum (plus/typed #'(number a) #'(number b))))
(pretty-print (syntax->datum (plus/typed #'(string a) #'(string b))))
(pretty-print (syntax->datum (minus/typed #'(number a) #'(number b))))

(pretty-print (syntax->datum (plus 1 2)))

(pretty-print (syntax->datum (plus "foo" "bar")))

(pretty-print (syntax->datum (plus 2 (plus 2 3))))
