*     X04BAF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      CHARACTER*40     REC
*     .. External Subroutines ..
      EXTERNAL         X04BAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'X04BAF Example Program Results'
      WRITE (NOUT,*)
      WRITE (REC,99999) 'This record was output by X04BAF'
*
      CALL X04BAF(NOUT,REC)
*
      STOP
*
99999 FORMAT (1X,A)
      END
