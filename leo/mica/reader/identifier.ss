(library (leo mica reader identifier)
  (export
    identifier
    identifier-string
    digit-char)
  (import
    (prefix (micascheme) %)
    (only (micascheme) define lambda read open-input-string always default)
    (mica reader))

  (define letter-char
    (one-of
      (range-char #\a #\z)
      (range-char #\A #\Z)))

  (define digit-char
    (range-char #\0 #\9))

  (define constituent-char
    (one-of
      letter-char
      (first-char
        (> #\delete)
        (not #\:)
        (category-char Lu Ll Lt Lm Lo Mn Nl No Pd Pc Po Sc Sm Sk So Co))))

  (define special-initial-char
    (one-of
      (char !)
      (char $)
      (char %)
      (char &)
      (char *)
      (char /)
      (char <)
      (char =)
      (char >)
      (char ?)
      (char ^)
      (char _)
      (char ~)))

  (define special-subsequent-char
    (one-of
      (char +)
      (char -)
      (char dot)
      (char at)))

  (define initial-string
    (one-of
      (string constituent-char)
      (string special-initial-char)
      ; inline-hex-escape-string
      ))

  (define subsequent-string
    (one-of
      initial-string
      (string digit-char)
      (string (category-char Nd Mc Me))
      (string special-subsequent-char)))

  (define subsequent-list-string
    (list-string (list subsequent-string)))

  (define peculiar-identifier-string
    (one-of "+" "..."
      (string-append "-"
        (or
          (optional (string-append ">" subsequent-list-string))
          (return "")))))

  (define identifier-string
    (one-of
      (string-append initial-string subsequent-list-string)
      peculiar-identifier-string))

  (define identifier
    (string->datum identifier-string))
)
