(import (scheme) (micac model) (micac syntax) (micac compiler) (check))

(check (equal? (type->datum (syntax->type #'bool)) `(type bool 1)))
(check (equal? (type->datum (syntax->type #'u8)) `(type u8 1)))
(check (equal? (type->datum (syntax->type #'u16)) `(type u16 2)))
(check (equal? (type->datum (syntax->type #'u32)) `(type u32 4)))

(check
  (equal?
    (body->datum
      (compiler-syntax->body
        (compiler #f)
        #'(
          (var u16 start)
          (var u32 mid)
          (var u8 end))))
    '(body
      (
        (variable start (type u16 2) 0)
        (variable mid (type u32 4) 2)
        (variable end (type u8 1) 6))
      ()
      7)))
