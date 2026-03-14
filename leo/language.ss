(library (leo language)
  (export leo-language)
  (import
    (micascheme)
    (language)
    (leo read)
    (leo expand))

  (define leo-language
    (language "leo" make-leo-read leo-expand))
)
