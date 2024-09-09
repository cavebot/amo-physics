*     X04AAF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. External Subroutines ..
      EXTERNAL         DUMMY, X04AAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'X04AAF Example Program Results'
*
      CALL X04AAF(1,NOUT)
      CALL DUMMY
*
      STOP
      END
*
      SUBROUTINE DUMMY
*     .. Local Scalars ..
      INTEGER          NERR
*     .. External Subroutines ..
      EXTERNAL         X04AAF
*     .. Executable Statements ..
      CALL X04AAF(0,NERR)
      WRITE (NERR,*)
      WRITE (NERR,*) 'This is a dummy error message'
      RETURN
      END
