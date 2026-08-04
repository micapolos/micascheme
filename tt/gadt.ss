(import
  (tt lang)
  (tt number)
  (tt string)
  (tt datum)
  (prefix (scheme) %))

(define-class (term _))

(define num
  (unchecked
    (pi (number) (term number))
    (%lambda (x) (%cons 0 x))))

(define str
  (unchecked
    (pi (string) (term string))
    (%lambda (x) (%cons 1 x))))

(define num+
  (unchecked
    (pi ((term number) (term number)) (term number))
    (%lambda (x y) (%cons 2 (%cons x y)))))

(define str+
  (unchecked
    (pi ((term string) (term string)) (term string))
    (%lambda (x y) (%cons 3 (%cons x y)))))

(define strlen
  (unchecked
    (pi ((term string)) (term number))
    (%lambda (x) (%cons 4 x))))

(define term-match
  (unchecked
    (forall (x r)
      (pi
        (
          (term x)
          (pi (number) r)
          (pi (string) r)
          (pi ((term number) (term number)) r)
          (pi ((term string) (term string)) r)
          (pi ((term string)) r))
        r))
    (%lambda (x num str + str+ strlen)
      (%case (%car x)
        ((0) (num (%cdr x)))
        ((1) (str (%cdr x)))
        ((2) (num+ (%cadr x) (%cddr x)))
        ((3) (str+ (%cadr x) (%cddr x)))
        ((4) (strlen (%cdr x)))))))

; (define (term->datum (forall t) (x (term t)))
;   (recursive datum
;     (term-match x
;       (lambda ((n number))
;         (datum-append 'num (number->datum n)))
;       (lambda ((s string))
;         (datum-append 'str (string->datum s)))
;       (lambda ((a (term number)) (b (term number)))
;         (datum-append '+ (term->datum a) (term->datum b)))
;       (lambda ((a (term string)) (b (term string)))
;         (datum-append 'str+ (term->datum a) (term->datum b)))
;       (lambda ((a (term string)))
;         (datum-append 'strlen (term->datum a))))))

; (define (term->datum (forall t) (x (term t)))
;   (match x
;     ((num n)
;       (datum-append 'num (number->datum n)))
;     ((str s)
;       (datum-append 'str (string->datum s)))
;     ((num+ a b)
;       (datum-append '+ (term->datum a) (term->datum b)))
;     ((str+ a b)
;       (datum-append 'str+ (term->datum a) (term->datum b)))
;     ((strlen a)
;       (datum-append 'strlen (term->datum a)))))

(define term->datum
  (unchecked
    (forall (x) (pi ((term x)) datum))
    (%rec term->datum (%lambda (x)
      (%case (%car x)
        ((0) (%list (%quote num) (%cdr x)))
        ((1) (%list (%quote str) (%cdr x)))
        ((2) (%list (%quote num+) (term->datum (%cadr x)) (term->datum (%cddr x))))
        ((3) (%list (%quote str+) (term->datum (%cadr x)) (term->datum (%cddr x))))
        ((4) (%list (%quote strlen) (term->datum (%cdr x)))))))))

(print (term->datum (strlen (str+ (str "foo") (str "bar")))))
