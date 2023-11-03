(import
  (micascheme)
  (tico type)
  (tico typed))

(check (typed-dynamic? (typed (native-type) '$foo)))
(check (not (typed-dynamic? (typed (value-type "foo") '$foo))))

(check
  (equal?
    (typed-list->dynamic-values
      (list (typed (value-type "foo") 'static)))
    (list)))

(check
  (equal?
    (typed-list->dynamic-values
      (list
        (typed (native-type) 'native1)
        (typed (value-type "foo") 'static)
        (typed (native-type) 'native2)))
    (list 'native1 'native2)))
