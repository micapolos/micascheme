(import (micascheme) (asm expression))

(check
  (equal?
    (expression->datum (expression (list #'a #'b) #'(+ a b)))
    '(expression (a b) (+ a b))))

(check
  (equal?
    (expression->datum (expression (list #'a #'b) #'(- a b)))
    '(expression (a b) (- a b))))

(check
  (equal?
    (expression->datum (expression (list #'a) #'(u8 a)))
    '(expression (a) (u8 a))))

(check
  (equal?
    (expression->datum (expression (list #'a) #'(u16 a)))
    '(expression (a) (u16 a))))

(check
  (equal?
    (expression->datum (syntax->expression (list) #'123))
    '(expression () 123)))

(check
  (equal?
    (expression->datum (syntax->expression (list) #'foo))
    '(expression (foo) foo)))

(check
  (equal?
    (expression->datum (syntax->expression (list #'foo) #'foo))
    '(expression () foo)))

(check
  (equal?
    (expression->datum (syntax->expression (list #'foo) #'(+ foo bar)))
    '(expression (bar) (+ foo bar))))
