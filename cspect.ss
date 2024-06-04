(library (cspect)
  (export cspect)
  (import (scheme))

  (define (cspect $path)
    (system
      (string-append
        "mono ~/Downloads/CSpect2_19_7_1/CSpect.exe -w5 -tv -zxnext "
        $path)))
)
