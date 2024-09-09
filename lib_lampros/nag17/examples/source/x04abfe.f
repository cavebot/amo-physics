*     X04ABF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. External Subroutines ..
      EXTERNAL         DUMMY, X04ABF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'X04ABF Example Program Results'
*
      CALL X04ABF(1,NOUT)
      CALL DUMMY
*
      STOP
      END
*
      SUBROUTINE DUMMY
*     .. Local Scalars ..
      INTEGER          NADV
*     .. External Subroutines ..
      EXTERNAL         X04ABF
*     .. Executable Statements ..
      CALL X04ABF(0,NADV)
      WRITE (NADV,*)
      WRITE (NADV,*) 'This is a dummy advisory message'
      RETURN
      END
