(import (micascheme) (tico))

; types

(check
  (equal?
    (typed-unsyntax (parse-typed #`type))
    (typed (box (type-type)) `(type-type) (type-type))))

(check
  (equal?
    (typed-unsyntax (parse-typed #`boolean))
    (typed (box (boolean-type)) `(boolean-type) (boolean-type))))

(check
  (equal?
    (typed-unsyntax (parse-typed #`number))
    (typed (box (number-type)) `(number-type) (number-type))))

(check
  (equal?
    (typed-unsyntax (parse-typed #`string))
    (typed (box (string-type)) `(string-type) (string-type))))

; literals

(check
  (equal?
    (typed-unsyntax (parse-typed #`#f))
    (typed (box #f) #f (boolean-type))))

(check
  (equal?
    (typed-unsyntax (parse-typed #`128))
    (typed (box 128) 128 (number-type))))

(check
  (equal?
    (typed-unsyntax (parse-typed #`"foo"))
    (typed (box "foo") "foo" (string-type))))

(check
  (equal?
    (typed-unsyntax (parse-typed #`(foo #t 128 "foo")))
    (typed
      (box (struct `foo (list #t 128 "foo")))
      `(struct 'foo (list #t 128 "foo"))
      (struct `foo (list (boolean-type) (number-type) (string-type))))))

; begin

(with-generate-temporary-seed $tmp
  (check
    (equal?
      (typed-unsyntax
        (parse-typed
          #`(begin 128)))
      (typed
        (box 128)
        128
        (number-type)))))

(with-generate-temporary-seed $tmp
  (check
    (equal?
      (typed-unsyntax
        (parse-typed
          #`(begin
            128
            (get number))))
      (typed
        (box 128)
        `(let (($tmp-0 128)) $tmp-0)
        (number-type)))))

(with-generate-temporary-seed $tmp
  (check
    (equal?
      (typed-unsyntax
        (parse-typed
          #`(begin
            128
            "foo"
            (get number))))
      (typed
        (box 128)
        `(let (($tmp-0 128))
          (let (($tmp-1 "foo"))
            $tmp-0))
        (number-type)))))

(with-generate-temporary-seed $tmp
  (check
    (equal?
      (typed-unsyntax
        (parse-typed
          #`(begin
            128
            129
            (get number))))
      (typed
        (box 129)
        `(let (($tmp-0 128))
          (let (($tmp-1 129))
            $tmp-1))
        (number-type)))))

; get

(check
  (equal?
    (typed-unsyntax
      (parse-typed
        (lambda ($type)
          (and
            (equal? $type (struct `my (list (number-type))))
            (typed
              (box (struct `my (list 128)))
              #`(struct 'foo (list 128))
              (struct `foo (list (number-type))))))
        #`(get (my number))))
    (typed
      (box (struct `my (list 128)))
      `(struct 'foo (list 128))
      (struct `foo (list (number-type))))))
