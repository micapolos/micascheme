library
  leo mica reader identifier
  export identifier string
  import
    leo scheme
    mica reader
  define letter char
    one of
      range: char a, char z
      range: char A, char Z
  define digit char
    range: char 0, char 9
  define constituent char
    one of
      letter char
      first char
        greater than char delete
        not char colon
        char category: Lu, Ll, Lt, Lm, Lo, Mn, Nl, No, Pd, Pc, Po, Sc, Sm, Sk, So, Co
  define special initial char
    one of
      char !
      char $
      char %
      char &
      char *
      char /
      char <
      char =
      char >
      char ?
      char ^
      char _
      char ~
  define special subsequent char
    one of
      char +
      char -
      char dot
      char at
  define initial string
    one of
      string constituent char
      string special initial char
      inline hex escape string
  define subsequent string
    one of
      initial string
      string digit char
      string category char: Nd, Mc, Me
      string special subsequent char
  define subsequent list string
    string list subsequent string
  define peculiar identifier string
    one of: "+", "...", peculiar dash identifier string
  define peculiar dash identifier string
    append: "-", peculiar dash identifier suffix string
  define peculiar dash identifier suffix string
    or
      optional string append: ">", subsequent list string
      ""
  define identifier string
    one of
      string append: initial string, subsequent list string
      peculiar identifier string
  define identifier
    datum identifier string
