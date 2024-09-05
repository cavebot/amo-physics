*     X05AAF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Arrays ..
      INTEGER          ITIME(7)
*     .. External Subroutines ..
      EXTERNAL         X05AAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'X05AAF Example Program Results'
*
      CALL X05AAF(ITIME)
*
      WRITE (NOUT,99999) '       Year : ', ITIME(1)
      WRITE (NOUT,99999) '      Month : ', ITIME(2)
      WRITE (NOUT,99999) '        Day : ', ITIME(3)
      WRITE (NOUT,99999) '       Hour : ', ITIME(4)
      WRITE (NOUT,99999) '     Minute : ', ITIME(5)
      WRITE (NOUT,99999) '     Second : ', ITIME(6)
      WRITE (NOUT,99999) 'Millisecond : ', ITIME(7)
      STOP
*
99999 FORMAT (1X,A,I4)
      END
