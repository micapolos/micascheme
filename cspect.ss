(library (cspect)
  (export cspect)
  (import (scheme) (blob))

  (define (cspect $path)
    (system
      (string-append
        "mono /Applications/CSpect/CSpect.exe -w5 -tv -exit -esc -brk -zxnext -60 -vsync -fps "
        $path)))
)
