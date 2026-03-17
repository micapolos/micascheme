import
  leo scheme
  bytevector

write bytevector
  10, 20, 30
  40, 50, 60
  128
  129
  255

write u16-bytevector: 3631, endianness little
write u16-bytevector: 3631, endianness big
