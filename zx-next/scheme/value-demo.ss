(import
  (zx-next core)
  (zx-next scheme value))

(run
  (value-ld #x01c000)
  (break)
  (value-push)
  (value-pop)
  (call value-ref)
  (break))
