(library (cspect)
  (export cspect)
  (import (scheme) (blob))

  (define cspect
    (case-lambda
      (($path)
        (system
          (string-append
            "mono /Applications/CSpect/CSpect.exe -w6 -tv -exit -esc -brk -zxnext -60 -vsync -fps -16bit "
            $path)))
      (($path $map-path)
        (system
          (string-append
            "mono /Applications/CSpect/CSpect.exe -w6 -tv -exit -esc -brk -zxnext -60 -vsync -fps -16bit -map="
            $map-path
            " "
            $path)))))
)
