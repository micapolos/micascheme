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

(with-vstack (sp 32)
  (vstack-let sp
    (p1 point)
    (p2 point)
    (begin
      (ftype-set! point (x) p1 10)
      (ftype-set! point (y) p1 20)
      (ftype-set! point (x) p2 30)
      (ftype-set! point (y) p2 40)
      (vstack-let sp
        (p3 point)
        (begin
          (ftype-set! point (x) p3
            (+
              (ftype-ref point (x) p1)
              (ftype-ref point (x) p2)))
          (ftype-set! point (y) p3
            (+
              (ftype-ref point (y) p1)
              (ftype-ref point (y) p2)))
          (ftype-set! point (x) p1
            (ftype-ref point (x) p3))
          (ftype-set! point (y) p1
            (ftype-ref point (y) p3))))
      (check (= (ftype-ref point (x) p1) 40))
      (check (= (ftype-ref point (y) p1) 60))
      (check (= (ftype-ref point (x) p2) 30))
      (check (= (ftype-ref point (y) p2) 40)))))
