(import (scheme) (check) (vstack))

(with-vstack (sp 16)
  (with-vstack-alloc (sp 4)
    (vstack-u32-set! sp 0 #x01234567)
    (check (= (vstack-u32-ref sp 0) #x01234567))
    (with-vstack-alloc (sp 4)
      (vstack-u32-set! sp 0 #x89abcdef)
      (check (= (vstack-u32-ref sp 0) #x89abcdef))
      (check (= (vstack-u32-ref sp 4) #x01234567)))
    (check (= (vstack-u32-ref sp 0) #x01234567))))

(define-ftype point (struct (x int) (y int)))

(define (point-x p) (foreign-ref 'int p 0))
(define (point-y p) (foreign-ref 'int p 4))
(define (point-set-x! p x) (foreign-set! 'int p 0 x))
(define (point-set-y! p y) (foreign-set! 'int p 4 y))

(with-vstack (sp 32)
  (vstack-let sp
    (p1 (ftype-sizeof point))
    (p2 (ftype-sizeof point))
    (begin
      (point-set-x! p1 10)
      (point-set-y! p1 20)
      (point-set-x! p2 30)
      (point-set-y! p2 40)
      (vstack-let sp
        (p3 (ftype-sizeof point))
        (begin
          (point-set-x! p3 (+ (point-x p1) (point-x p2)))
          (point-set-y! p3 (+ (point-y p1) (point-y p2)))
          (point-set-x! p1 (point-x p3))
          (point-set-y! p1 (point-y p3))))
      (check (= (point-x p1) 40))
      (check (= (point-y p1) 60))
      (check (= (point-x p2) 30))
      (check (= (point-y p2) 40)))))
