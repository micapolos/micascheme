(library (nex)
  (export nex-blob)
  (import (micascheme))

  (define (u16-blob $u16)
    (blob 2
      (lambda ($port)
        (put-u16 $port $u16 (endianness little)))))

  (define nex-blob
    (case-lambda
      (($asm-blob)
        (nex-blob $asm-blob #xc000))
      (($asm-blob $start)
        (blob-append
          (bytevector->blob (string->utf8 "Next"))
          (bytevector->blob (string->utf8 "v1.2"))
          (u8-blob 0) ; required 768k RAM
          (u8-blob 1) ; number of banks
          (u8-blob 0) ; loading screen blocks
          (u8-blob 0) ; border color
          (u16-blob #xbfe0) ; SP
          (u16-blob $start) ; PC
          (u16-blob 0) ; number of extra files
          (u8-blob 1) ; bank0
          (bytevector->blob (make-bytevector 111 0)) ; other banks empty
          (u8-blob 0) ; loading bar?
          (u8-blob 0) ; loading bar color
          (u8-blob 0) ; loading delay per bank
          (u8-blob 0) ; start delay
          (u8-blob 0) ; preserve next registers
          (u8-blob 0 0 0) ; required core version
          (u8-blob 0) ; timex hi-res
          (u8-blob 0) ; entry bank in slot 3
          (u16-blob 0) ; file handle address
          (bytevector->blob (make-bytevector (- 512 142) 0))
          ; data and padding
          $asm-blob
          (bytevector->blob (make-bytevector (- #x4000 (blob-size $asm-blob)) 0))))))
 )
