*     X04BBF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION X
      INTEGER          I, IFAIL
      CHARACTER*40     REC
*     .. External Subroutines ..
      EXTERNAL         X04BBF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'X04BBF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      WRITE (NOUT,*)
*
*     Read in values of I and X.
*
      CALL X04BBF(NIN,REC,IFAIL)
*
      READ (REC,99999) I, X
*
*     Write out I and X.
      WRITE (NOUT,99998) I, X
      STOP
*
99999 FORMAT (I3,F7.3)
99998 FORMAT (1X,I5,F11.3)
      END
