(import
  (scheme)
  (check)
  (lets)
  (procedure)
  (boolean)
  (asm8 type))

(define unsigned-8 (make-primitive-type (gensym) 'unsigned-8 1 1))
(define unsigned-16 (make-primitive-type (gensym) 'unsigned-16 2 2))

(define vec4-type
  (make-array-type (gensym) (gensym) 2 8
    4 unsigned-16))

(define point-type
  (make-struct-type (gensym) 'point 2 4
    (vector
      (make-field 'x (make-location 0 unsigned-16))
      (make-field 'y (make-location 2 unsigned-16)))))

(check (type? point-type))
(check (struct-type? point-type))

(lets
  ($location (struct-ref? point-type 'x))
  (run
    (check (= (location-offset $location) 0))
    (check (eq? (location-type $location) unsigned-16))))

(lets
  ($location (struct-ref? point-type 'y))
  (run
    (check (= (location-offset $location) 2))
    (check (eq? (location-type $location) unsigned-16))))

(check (false? (struct-ref? point-type 'z)))
