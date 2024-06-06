(import (scheme) (cspect) (procedure))

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
    (put-utf8 $port "Next")
    (put-utf8 $port "V1.0")
    (put-u8   $port 1) ; required-ram
    (put-u8   $port 1) ; number of banks to load
    (put-u8   $port #b00000001) ; loading screen (Layer-2 without palette)
    (put-u8   $port 3) ; border color
    (put-u16  $port #x0000) ; stack pointer
    (put-u16  $port #xc000) ; program pointer
    (put-u16  $port 0) ; extra files
    (repeat-indexed ($index 112)
      (put-u8 $port
        (case $index
          ((5) #xff)
          (else 0))))
    (put-u8    $port 0) ; layer-2 loading bar
    (put-u8    $port 0) ; loading bar color
    (put-u8    $port 0) ; loading delay
    (put-u8    $port 0) ; start delay
    (put-u8    $port 0) ; preserve current registers
    (put-u8    $port 0) (put-u8 $port 0) (put-u8 $port 0) ; required core version
    (put-u8    $port 0) ; timex hi-res
    (put-u8    $port 5) ; entry bank
    (put-u16   $port 0) ; file handle address
    (put-zeros $port (- 512 142))

    ; palette (512)
    (repeat-indexed ($index 512)
      (put-u8 $port (fxand $index #xff)))

    ; loading screen (Layer-2)
    (repeat-indexed ($index 49152)
      (put-u8 $port (fxand $index #x01)))

    ; loading screen (ULA)
    ; (repeat-indexed ($index 6912)
    ;   (put-u8 $port (fxand $index #xff)))

    ; bank 5
    (put-u8    $port #xf3) ; DI
    (put-u8    $port #x3e) ; LD A, 0
    (put-u8    $port #b00000010)
    (put-u8    $port #xd3) ; OUT ($fe), A
    (put-u8    $port #xfe)
    (put-u8    $port #x3e) ; LD A, $ff
    (put-u8    $port #b00010101)
    (put-u8    $port #xd3) ; OUT ($fe), A
    (put-u8    $port #xfe)
    (put-u8    $port #xc3) ; JMP $c001
    (put-u8    $port #x01)
    (put-u8    $port #xc0)
    (put-zeros $port (- #x4000 12))))

;(cspect "~/git/zxnext_sprite/demo/bin/sccz80/zxnext_sprite_demo.nex")
;(cspect "~/Documents/nextsync12/apps/Qbee.NEX")
(cspect $path)
