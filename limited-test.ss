(import (scheme) (check) (limited))

(check (make-limited? "foo" 2))
(check (make-limited? "foo" 0))
(check (not (make-limited? "foo" -1)))

(check
  (limited=? string=?
    (make-limited? "foo" 2)
    (make-limited? "foo" 2)))

(check
  (not
    (limited=? string=?
      (make-limited? "foo" 2)
      (make-limited? "foo" 3))))

(check
  (not
    (limited=? string=?
      (make-limited? "foo" 2)
      (make-limited? "bar" 2))))

(check
  (equal?
    (limited-use? (make-limited? "foo" 2) 1)
    (make-limited? "foo" 1)))

(check
  (equal?
    (limited-use? (make-limited? "foo" 2) 2)
    (make-limited? "foo" 0)))

(check (not (limited-use? (make-limited? "foo" 2) 3)))

