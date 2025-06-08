(import (scheme) (cspect) (procedure) (blob) (nex))

(define (put-utf8 $port $string)
  (put-bytevector $port (string->utf8 $string)))

(define (put-u16 $port $u16)
  (put-u8 $port (fxand $u16 #xff))
  (put-u8 $port (fxand (fxsrl $u16 8) #xff)))

(define (put-zeros $port $size)
  (repeat $size
    (put-u8 $port 0)))

(define $path "cspect.nex")

(call-with-port (open-file-output-port $path (file-options no-fail))
  (lambda ($port)
    (put-blob $port
      (nex-blob
        (u8-blob
          #xf3 ; DI
          #x3e ; LD A, 0
          #b00000010
          #xd3 ; OUT ($fe), A
          #xfe
          #x3e ; LD A, $ff
          #b00010101
          #xd3 ; OUT ($fe), A
          #xfe
          #xc3 ; JMP $c001
          #x01
          #xc0)))))

;(cspect "~/git/zxnext_sprite/demo/bin/sccz80/zxnext_sprite_demo.nex")
;(cspect "~/Documents/nextsync12/apps/Qbee.NEX")
(cspect $path)
