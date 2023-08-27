(import (micascheme) (leo2) (leo compiler))

(check 
  (obj=?
    (syntax->datum
      (term-syntax
        (term-bind
          (term #`(values "foo" 128) (list string! number!))
          (lambda ($bindings)
            (term #`(values #,@(reverse (map binding-identifier $bindings)))
              (list (struct! result)))))))
    `(let-values ((($string $number) (values "foo" 128))) 
      (values $string $number))))
