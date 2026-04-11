(import
  (scheme)
  (foreign)
  (syntaxes))

; conclusion
; the fastest way to access memory on 64-bit architecture
; is access using s64 / iptr and truncating to 60-bits ignoring the sign.

(let*
  (
    (int-size 8)
    (mask #xfffffffffffffff)
    (bytevector-ref bytevector-s64-native-ref)
    (bytevector-set! bytevector-s64-native-set!)
    ($foreign-type 'iptr)
    ($size (string->number (car (command-line-arguments))))
    ($bytevector (make-immobile-bytevector (fx*/wraparound $size int-size) 0))
    ($address (object->reference-address $bytevector)))
  (display "Bytevector\n")
  (bytevector-set! $bytevector (fx*/wraparound (fx-/wraparound $size 1) int-size) 1)
  (write
    (time
      (let loop
        (($index (fx*/wraparound (fx-/wraparound $size 1) int-size)))
        (cond
          ((fxzero? $index)
            (bytevector-ref $bytevector $index))
          (else
            (let (($new-index (fx-/wraparound $index int-size)))
              (bytevector-set! $bytevector $new-index
                (logand
                  (fx*/wraparound
                    (bytevector-ref $bytevector $index)
                    123)
                  mask))
              (loop $new-index)))))))
  (newline)

  (display "Foreign\n")
  (bytevector-set! $bytevector (fx*/wraparound (fx-/wraparound $size 1) int-size) 1)
  (write
    (time
      (let loop
        (($index (fx* (fx-/wraparound $size 1) int-size)))
        (cond
          ((fxzero? $index)
            (foreign-ref $foreign-type $address $index))
          (else
            (let (($new-index (fx-/wraparound $index int-size)))
              (foreign-set! $foreign-type $address $new-index
                (logand
                  (fx*/wraparound
                    (foreign-ref $foreign-type $address $index)
                    123)
                  mask))
              (loop $new-index)))))))
  (newline))
