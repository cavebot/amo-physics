*     X05ABF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      CHARACTER*30     CTIME
*     .. Local Arrays ..
      INTEGER          ITIME(7)
*     .. External Functions ..
      CHARACTER*30     X05ABF
      EXTERNAL         X05ABF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'X05ABF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) ITIME
*
      CTIME = X05ABF(ITIME)
*
      WRITE (NOUT,99999) CTIME
      STOP
*
99999 FORMAT (1X,A)
      END
