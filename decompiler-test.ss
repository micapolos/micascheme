(import (micascheme) (decompiler) (term) (typed))

(define-syntax-rule (check-decompile $typed $expected)
  (check (obj=? (decompile $typed) $expected)))

(check-decompile (typed! #f) #f)
(check-decompile (typed! 128) 128)
(check-decompile (typed! "foo") "foo")

(check-decompile (typed! boolean) boolean!)
(check-decompile (typed! number) number!)
(check-decompile (typed! string) string!)
(check-decompile (typed! type) type!)
(check-decompile (typed! foo) `foo)

(check-decompile 
  (typed-tuple! (foo)) 
  (tuple-type! (foo)))

(check-decompile 
  (typed-tuple! (foo (typed! 128))) 
  (tuple-type! (foo 128)))

(check-decompile 
  (typed-tuple! (foo (typed! 128) (typed! "foo"))) 
  (tuple-type! (foo 128 "foo")))

(check-decompile 
  (typed-tuple! (foo (typed! 128) (typed! "foo") (typed! #t))) 
  (tuple-type! (foo 128 "foo" #t)))

(check-decompile 
  (typed-tuple! (choice (typed! number) (typed! string) (typed! boolean)))
  (choice-type! number! string! boolean!))
