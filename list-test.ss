(import (scheme) (list) (check) (data) (lets) (stack) (indexed) (generate) (failure) (procedure))

; === binder ===

(lets
  ((bindable-list a b . c) (list "a" "b" "c" "d"))
  (run
    (check (equal? a "a"))
    (check (equal? b "b"))
    (check (equal? c (list "c" "d")))))

; === single ===

(check (equal? (single? (list)) #f))
(check (equal? (single? (list "a")) #t))
(check (equal? (single? (list "a" "b")) #f))

(check (equal? (single (list)) #f))
(check (equal? (single (list "a")) "a"))
(check (equal? (single (list "a" "b")) #f))

(with-generate-temporary-seed tmp
  (check
    (equal?
      (generate-symbols 3)
      (list `tmp-0 `tmp-1 `tmp-2))))

; === list-safe-ref ===

(check (equal? (list-get (list "a" "b") 0) "a"))
(check (equal? (list-get (list "a" "b") 1) "b"))
(check (equal? (list-get (list "a" "b") 2) (list-get-overflow 0)))
(check (equal? (list-get (list "a" "b") 3) (list-get-overflow 1)))

; === bind-if ===

(check (equal? (bind-if string? "foo" string-length) 3))
(check (equal? (bind-if string? 128 string-length) 128))

; === fold-while ===

(check
  (equal?
    (fold-while string?
      (lambda ($string $char)
        (if (char-alphabetic? $char)
          (string-append $string (string $char))
          (failure $char)))
      ""
      (list #\a #\b))
    "ab"))

(check
  (equal?
    (fold-while string?
      (lambda ($string $char)
        (if (char-alphabetic? $char)
          (string-append $string (string $char))
          (failure $char)))
      ""
      (list #\a #\b #\1 #\2 #\3))
    (failure #\1)))

; === associ ===

(check (equal? (associ (list) 100 `a) #f))
(check (equal? (associ (list (cons `a `foo) (cons `b `bar)) 100 `a) (cons 100 `foo)))
(check (equal? (associ (list (cons `a `foo) (cons `b `bar)) 100 `b) (cons 101 `bar)))
(check (equal? (associ (list (cons `a `foo) (cons `b `bar)) 100 `c) #f))

; === map-find-indexed ===

(let ((fn (lambda (s) (and (> (string-length s) 3) (string-append s "!")))))
  (check (equal? (map-find-indexed fn (list "ala" "ma" "kocicę" "Lunę")) (indexed "kocicę!" 2)))
  (check (equal? (map-find-indexed fn (list "ala" "ma" "ul")) #f)))

; === indexed-find ===

(let ()
  (define $proc
    (lambda ($index $string)
      (and
        (= (string-length $string) 3)
        (cons $index $string))))
  (check
    (equal?
      (indexed-find $proc (list "Luna" "kot" "lis"))
      (cons 1 "kot")))
  (check
    (equal?
      (indexed-find $proc (list "Luna" "pies"))
      #f)))

; === intercalate ===

(check (equal? (intercalate (list) 0) (list)))
(check (equal? (intercalate (list 1) 0) (list 1)))
(check (equal? (intercalate (list 1 2 3) 0) (list 1 0 2 0 3)))

; === indices ===

(check (equal? (indices 3) (list 0 1 2)))

; === filter-map ===

(check (equal? (filter-map (lambda (a b) (and a b)) (list #f #t #f #t) (list 1 2 3 4)) (list 2 4)))

; === filter-opts ===

(check (equal? (filter-opts (list 1 #f "foo" #f #t)) (list 1 "foo" #t)))

; === opt-lift ===

(check (equal? (opt-lift list "a" "b") (list "a" "b")))
(check (equal? (opt-lift list #f "b") #f))
(check (equal? (opt-lift list "a" #f) #f))
(check (equal? (opt-lift list #f #f) #f))

(check (equal? (opt-lift list (opt "a") (opt "b")) (list "a" "b")))
(check (equal? (opt-lift list (opt #f) (opt "b")) (list #f "b")))
(check (equal? (opt-lift list (opt "a") #f) #f))

; === indexed ===

(check (equal? (indexed-value (indexed "a" 1)) "a"))
(check (equal? (indexed-index (indexed "a" 1)) 1))

; === map-indexed ===

(check (equal? (map-indexed cons (list "a" "b" "c")) (list (cons 0 "a") (cons 1 "b") (cons 2 "c"))))

; === list-ref-opt ===

(check (equal? (list-ref-opt (list "a" "b") 0) "a"))
(check (equal? (list-ref-opt (list "a" "b") 1) "b"))
(check (equal? (list-ref-opt (list "a" "b") 2) #f))

; === list-drop ===

(check (equal? (list-drop (list "a" "b") 0) (list "a" "b")))
(check (equal? (list-drop (list "a" "b") 1) (list "b")))
(check (equal? (list-drop (list "a" "b") 2) (list)))
(check (equal? (list-drop (list "a" "b") 3) #f))

; === list-indexed ===

(check (equal? (list-indexed (list "a" "b" "c")) (list (indexed "a" 0) (indexed "b" 1) (indexed "c" 2))))

; === null ===

(check (equal? null `()))

; === ordered-map ===

(check
  (equal?
    (with-tmps
      (ordered-map
        (lambda (_) (generate-symbol))
        (indices 8)))
    '($tmp-0 $tmp-1 $tmp-2 $tmp-3 $tmp-4 $tmp-5 $tmp-6 $tmp-7)))

; === build-list ===

(check
  (equal?
    (build-list 5 add1)
    (list 1 2 3 4 5)))

; === ensure-list ===

(check (equal? (ensure-list 1) (list 1)))
(check (equal? (ensure-list (cons 1 2)) (list (cons 1 2))))

(check (equal? (ensure-list (list)) (list)))
(check (equal? (ensure-list (list 1)) (list 1)))
(check (equal? (ensure-list (list 1 2 3)) (list 1 2 3)))

; === values->list ===

(check (equal? (values->list (values)) (list)))
(check (equal? (values->list (values 1 2 3)) (list 1 2 3)))

; === assp-update ===

(check
  (equal?
    (assp-update
      (partial eq? 'foo)
      add1
      '(
        (foo . 10)
        (bar . 20)))
    '(
      (foo . 11)
      (bar . 20))))

(check
  (equal?
    (assp-update
      (partial eq? 'bar)
      add1
      '(
        (foo . 10)
        (bar . 20)))
    '(
      (foo . 10)
      (bar . 21))))

(check
  (equal?
    (assp-update
      (partial eq? 'goo)
      add1
      '(
        (foo . 10)
        (bar . 20)))
    #f))

; === assp-update-new ===

(check
  (equal?
    (assp-update-new
      (partial eq? 'foo)
      add1
      (lambda () '(foo . 30))
      '(
        (foo . 10)
        (bar . 20)))
    '(
      (foo . 11)
      (bar . 20))))

(check
  (equal?
    (assp-update-new
      (partial eq? 'bar)
      add1
      (lambda () '(bar . 30))
      '(
        (foo . 10)
        (bar . 20)))
    '(
      (foo . 10)
      (bar . 21))))

(check
  (equal?
    (assp-update-new
      (partial eq? 'goo)
      add1
      (lambda () '(goo . 30))
      '(
        (foo . 10)
        (bar . 20)))
    '(
      (goo . 31)
      (foo . 10)
      (bar . 20))))

; === assoc-update ===

(check
  (equal?
    (assoc-update 'foo add1
      '(
        (foo . 10)
        (bar . 20)))
    '(
      (foo . 11)
      (bar . 20))))

(check
  (equal?
    (assoc-update 'bar add1
      '(
        (foo . 10)
        (bar . 20)))
    '(
      (foo . 10)
      (bar . 21))))

(check
  (equal?
    (assoc-update 'goo add1
      '(
        (foo . 10)
        (bar . 20)))
    #f))

; === assoc-update-new ===

(check
  (equal?
    (assoc-update-new 'foo add1 (lambda () 30)
      '(
        (foo . 10)
        (bar . 20)))
    '(
      (foo . 11)
      (bar . 20))))

(check
  (equal?
    (assoc-update-new 'bar add1 (lambda () 30)
      '(
        (foo . 10)
        (bar . 20)))
    '(
      (foo . 10)
      (bar . 21))))

(check
  (equal?
    (assoc-update-new 'goo add1 (lambda () 30)
      '(
        (foo . 10)
        (bar . 20)))
    '(
      (goo . 31)
      (foo . 10)
      (bar . 20))))

; === group-by ===

(check
  (equal?
    (group-by car eq?
      `(
        (foo . 10)
        (bar . 20)
        (foo . 30)))
    `(
      (foo .
        (
          (foo . 10)
          (foo . 30)))
      (bar .
        (
          (bar . 20))))))
