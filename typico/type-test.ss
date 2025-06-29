(import (micascheme) (typico type))

(define type-a (primitive-type 'a 'a))
(define type-b (primitive-type 'b 'b))
(define type-c (primitive-type 'c 'c))

(check (type? (primitive-type (gensym) 'integer)))
(check (type? (function-type (list) (primitive-type (gensym) 'integer))))
(check (type? (expander-type (lambda ($lookup $syntax) (void)))))
(check (not (type? 123)))

; --- type=?

(check
  (type=?
    (primitive-type 'a 'foo)
    (primitive-type 'a 'bar)))

(check
  (not
    (type=?
      (primitive-type 'a 'foo)
      (primitive-type 'b 'foo))))

(check
  (type=?
    (function-type (list type-a type-b) type-c)
    (function-type (list type-a type-b) type-c)))

(check
  (type=?
    (function-type (list* type-a type-b) type-c)
    (function-type (list* type-a type-b) type-c)))

(check
  (not
    (type=?
      (function-type (list type-a) type-c)
      (function-type (list type-a type-b) type-c))))

(check
  (not
    (type=?
      (function-type (list type-a type-c) type-c)
      (function-type (list type-a type-b) type-c))))

(check
  (not
    (type=?
      (function-type (list type-a type-b) type-b)
      (function-type (list type-a type-b) type-c))))

(check
  (not
    (type=?
      (function-type (list type-a) type-c)
      (function-type (list type-a type-b) type-c))))

(check
  (not
    (type=?
      (function-type (list type-a type-c) type-c)
      (function-type (list type-a type-b) type-c))))

(check
  (not
    (type=?
      (function-type (list type-a type-b) type-b)
      (function-type (list* type-a type-b) type-c))))

(check
  (type=?
    (forall-type 2 type-a)
    (forall-type 2 type-a)))

(check
  (not
    (type=?
      (forall-type 1 type-a)
      (forall-type 2 type-a))))

(check
  (not
    (type=?
      (forall-type 2 type-a)
      (forall-type 2 type-b))))

(check
  (type=?
    (variable-type 1)
    (variable-type 1)))

(check
  (type=?
    (application-type type-a (list type-b type-c))
    (application-type type-a (list type-b type-c))))

(check
  (not
    (type=?
      (application-type type-b (list type-b type-c))
      (application-type type-a (list type-b type-c)))))

(check
  (not
    (type=?
      (application-type type-a (list type-b))
      (application-type type-a (list type-b type-c)))))

(check
  (not
    (type=?
      (application-type type-a (list type-b type-b))
      (application-type type-a (list type-b type-c)))))

(check
  (not
    (type=?
      (variable-type 1)
      (variable-type 2))))

(check
  (type=?
    (expander-type +)
    (expander-type +)))

(check
  (not
    (type=?
      (expander-type +)
      (expander-type -))))

; --- type->datum

(check
  (equal?
    (type->datum (primitive-type (gensym) 'foo))
    'foo))

(check
  (equal?
    (type->datum
      (function-type
        (list)
        (primitive-type (gensym) 'result)))
    '(-> result)))

(check
  (equal?
    (type->datum
      (function-type
        (list
          (primitive-type (gensym) 'a)
          (primitive-type (gensym) 'b))
        (primitive-type (gensym) 'result)))
    '(-> a b result)))

(check
  (equal?
    (type->datum
      (function-type
        (list* (primitive-type (gensym) 'a))
        (primitive-type (gensym) 'result)))
    '(-> a ... result)))

(check
  (equal?
    (type->datum
      (function-type
        (list*
          (primitive-type (gensym) 'a)
          (primitive-type (gensym) 'b))
        (primitive-type (gensym) 'result)))
    '(-> a b ... result)))

(check
  (equal?
    (type->datum (forall-type 3 (primitive-type (gensym) 'a)))
    '(forall 3 a)))

(check
  (equal?
    (type->datum (variable-type 3))
    '(variable 3)))

(check
  (equal?
    (type->datum (expander-type identity))
    `(expander ,identity)))
