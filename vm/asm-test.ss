(import (micascheme) (vm vm) (vm asm))

(define-syntax push-u8
  (make-compile-time-value
    (lambda ($vm $syntax)
      (syntax-case $syntax ()
        ((id value)
          #`(vm-u8-push! #,$vm value))))))

(lets
  ($vm (make-empty-vm 4))
  ($asm (asm (push-u8 13) (push-u8 12) (push-u8 11)))
  (run
    ($asm $vm)
    (check (equal? $vm (make-vm (bytevector 0 11 12 13) 1)))))
