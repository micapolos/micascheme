(import
  (zx-next core)
  (zx-next scheme value)
  (zx-next scheme pair))

(run
  (ld de #x0001)
  (ld hl #x1300)
  (break)
  (call ref)
  (break))
