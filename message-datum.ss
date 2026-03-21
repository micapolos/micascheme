(library (message-datum)
  (export message->datum)
  (import (scheme))

  (define (message->datum $message)
    (case $message
      (("misplaced aux keyword")
        `(misplaced (aux keyword)))
      (else
        `(message $message))))

)
