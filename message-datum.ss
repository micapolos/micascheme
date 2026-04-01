(library (message-datum)
  (export message->datum)
  (import (scheme))

  (define (message->datum $message)
    (case $message
      (("invalid syntax")
        '(invalid syntax))
      (("misplaced aux keyword")
        '(misplaced (aux keyword)))
      (else
        `(message ,$message))))

)
