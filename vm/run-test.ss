(import (micascheme) (vm vm) (vm run))

(define-syntax push-u8
  (make-compile-time-value
    (lambda ($vm $syntax)
      (syntax-case $syntax ()
        ((id value)
          #`(vm-u8-push! #,$vm value))))))

(lets
  ($vm (make-empty-vm 4))
  (run
    (vm-run $vm (push-u8 13))
    (vm-run $vm (push-u8 12) (push-u8 11))
    (check (equal? $vm (make-vm (bytevector 0 11 12 13) 1)))))
