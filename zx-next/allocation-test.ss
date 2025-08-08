(import (zx-next test) (zx-next allocation))

(define-fragments
  (bytes (bytes-allocation #x12 #x34 #x56 #x78))
  (string (db #x20) (ascii "This is string allocation!!!"))
  (pointers (pointers-allocation (#X00 #x1234) (#x40 #x5678) (#x61 #xabcd))))

(test
  (case bytes-allocation
    (assert-byte (bytes) #x04)
    (assert-byte ((+ bytes 1)) #x12)
    (assert-byte ((+ bytes 2)) #x34)
    (assert-byte ((+ bytes 3)) #x56)
    (assert-byte ((+ bytes 4)) #x78))

  (case write-allocation/bytes
    (write-allocation bytes))

  (case write-allocation/string
    (write-allocation string))

  (case write-allocation/pointers
    (write-allocation pointers))
)
