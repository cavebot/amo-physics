*     X05ACF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      INTEGER          K
      CHARACTER*50     CTIME1, CTIME2
*     .. External Functions ..
      INTEGER          X05ACF
      EXTERNAL         X05ACF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'X05ACF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) CTIME1, CTIME2
*
      K = X05ACF(CTIME1,CTIME2)
*
      IF (K.LT.0) THEN
         WRITE (NOUT,99999) CTIME1
         WRITE (NOUT,99999) 'is earlier than'
         WRITE (NOUT,99999) CTIME2
      ELSE IF (K.EQ.0) THEN
         WRITE (NOUT,99999) CTIME1
         WRITE (NOUT,99999) 'is equivalent to'
         WRITE (NOUT,99999) CTIME2
      ELSE
         WRITE (NOUT,99999) CTIME1
         WRITE (NOUT,99999) 'is later than'
         WRITE (NOUT,99999) CTIME2
      END IF
      STOP
*
99999 FORMAT (1X,A)
      END
