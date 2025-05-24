(library (sjasm run)
  (export sjasm-run)
  (import (micascheme) (sjasm code))

  (define (sjasm-run $string)
    (run
      (displayln $string)
      (call-with-output-file
        "/tmp/main.asm"
        (lambda ($port) (put-string $port $string))
        `(replace))
      (system "sjasmplus --zxnext=cspect --lst=/tmp/main.lst /tmp/main.asm")
      (system "cp /tmp/main.nex ~/Documents/nextsync12/apps/zexy/main.nex")
      (system "cspect -map=/tmp/main.map /tmp/main.nex")))
)
