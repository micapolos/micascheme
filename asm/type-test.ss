(import (micascheme) (asm type))

(define-syntax (type $syntax $lookup)
  (syntax-case $syntax ()
    ((_ expr)
      (type->syntax (syntax->type $lookup #'expr)))))

(define-primitive-type boolean)
(define-primitive-type integer)
(define-function-type function)

; === type->syntax

(check-datum=?
  (type->syntax (primitive-type 'a 'b))
  '(primitive-type 'a 'b))

(check-datum=?
  (type->syntax
    (function-type
      (list (primitive-type 'a 'a) (primitive-type 'b 'b))
      (primitive-type 'c 'c)))
  '(function-type
    (list* (primitive-type 'a 'a) (primitive-type 'b 'b) (list))
    (primitive-type 'c 'c)))

(check-datum=?
  (type->syntax
    (function-type
      (list* (primitive-type 'a 'a) (primitive-type 'b 'b))
      (primitive-type 'c 'c)))
  '(function-type
    (list* (primitive-type 'a 'a) (primitive-type 'b 'b))
    (primitive-type 'c 'c)))

; === type->datum

(check
  (equal?
    (type->datum (primitive-type (gensym) 'foo))
    'foo))

(check
  (equal?
    (type->datum
      (function-type
        (list)
        (primitive-type (gensym) 'c)))
    '(function () c)))

(check
  (equal?
    (type->datum
      (function-type
        (list
          (primitive-type (gensym) 'a)
          (primitive-type (gensym) 'b))
        (primitive-type (gensym) 'c)))
    '(function (a b) c)))

(check
  (equal?
    (type->datum
      (function-type
        (list* (primitive-type (gensym) 'a))
        (primitive-type (gensym) 'c)))
    '(function a c)))

(check
  (equal?
    (type->datum
      (function-type
        (list*
          (primitive-type (gensym) 'a)
          (primitive-type (gensym) 'b))
        (primitive-type (gensym) 'c)))
    '(function (a . b) c)))

; === type=?

(check (type=? (type boolean) (type boolean)))
(check (not (type=? (type integer) (type boolean))))

