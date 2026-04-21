(library (message-datum)
  (export message->datum)
  (import (scheme))

  (define (message->datum $message)
    (case $message
      (("invalid syntax")
        '(invalid syntax))
      (("misplaced aux keyword")
        '(misplaced (aux keyword)))
      (("attempt to reference unbound identifier")
        '(attempt (to (reference (unbound identifier)))))
      (("invalid top level program")
        '(invalid (top (level program))))
      (else
        `(message ,$message))))

)
