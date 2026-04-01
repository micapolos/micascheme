(import
  (scheme)
  (check)
  (terminal)
  (switch)
  (eof)
  (procedure))

;(enter-raw-mode)

; (let loop ()
;   (switch (get-char (console-input-port))
;     ((eof? _) (void))
;     ((else $char)
;       (case $char
;         ((#\x4) (void))
;         ((#\x1b)
;           (switch (get-char (console-input-port))
;             ((eof? _) (void))
;             ((else $char)
;               (run
;                 (display (format "(escape ~s)" (char->integer $char)))
;                 (put-char (console-output-port) $char)
;                 (loop)))))
;         ((#\return)
;           (put-char (console-output-port) #\return)
;           (put-char (console-output-port) #\newline)
;           (loop))
;         ((#\p)
;           (exit-raw-mode)
;           (put-string (console-output-port) (get-clipboard))
;           (enter-raw-mode)
;           (loop))
;         (else
;           (display (format "(~s)" (char->integer $char)))
;           (put-char (console-output-port) $char)
;           (loop))))))

;(exit-raw-mode)
