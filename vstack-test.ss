(import (scheme) (check) (vstack) (lets) (foreign) (syntax) (syntaxes))

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

(define/sp (princik)
  (pretty-print `(vstack (sp-min ,sp-min) (sp ,sp))))

(define-ftype
  (sdl-rect
    (struct
      (x int)
      (y int)
      (w int)
      (h int))))

(define (print-rect $rect)
  (pretty-print
    `(rect
      (x ,(foreign-int $rect 0))
      (y ,(foreign-int $rect 4))
      (w ,(foreign-int $rect 8))
      (h ,(foreign-int $rect 12)))))

(define-rules-syntax
  ((sdl-rect-set! lhs x y w h)
    (lets
      (rect lhs)
      (begin
        (foreign-set-int! rect 0 x)
        (foreign-set-int! rect 4 y)
        (foreign-set-int! rect 8 w)
        (foreign-set-int! rect 12 h))))
  ((sdl-rect-set! lhs rhs)
    (lets
      (rhs-rect rhs)
      (sdl-rect-set! lhs
        (foreign-int rhs-rect 0)
        (foreign-int rhs-rect 4)
        (foreign-int rhs-rect 8)
        (foreign-int rhs-rect 12)))))

(with/sp 16
  (lets
    ($rect (ftype/sp sdl-rect))
    (begin
      (sdl-rect-set! $rect 10 20 30 40)
      (check (= (foreign-int $rect 0) 10))
      (check (= (foreign-int $rect 4) 20))
      (check (= (foreign-int $rect 8) 30))
      (check (= (foreign-int $rect 12) 40)))))
