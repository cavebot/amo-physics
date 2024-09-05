*     P01ABF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION Y
      INTEGER          IFAIL
*     .. External Subroutines ..
      EXTERNAL         MYSQRT
*     .. Executable Statements ..
      WRITE (NOUT,*) 'P01ABF Example Program Results'
      WRITE (NOUT,*)
      WRITE (NOUT,*)
     +'Soft failure, silent exit - message output from the main program'
      IFAIL = 1
      CALL MYSQRT(-1.0D0,Y,IFAIL)
      IF (IFAIL.EQ.0) THEN
         WRITE (NOUT,99999) Y
      ELSE
         WRITE (NOUT,*)
     +     'Attempt to take the square root of a negative number'
      END IF
*
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Soft failure, noisy exit'
      IFAIL = -1
      CALL MYSQRT(-2.0D0,Y,IFAIL)
      IF (IFAIL.EQ.0) THEN
         WRITE (NOUT,99999) Y
      END IF
*
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Hard failure, noisy exit'
      IFAIL = 0
      CALL MYSQRT(-3.0D0,Y,IFAIL)
      WRITE (NOUT,99999) Y
      STOP
*
99999 FORMAT (1X,F10.4)
      END
*
      SUBROUTINE MYSQRT(X,Y,IFAIL)
*     Simple routine to compute square root
*     .. Parameters ..
      CHARACTER*6       SRNAME
      PARAMETER         (SRNAME='MYSQRT')
*     .. Scalar Arguments ..
      DOUBLE PRECISION  X, Y
      INTEGER           IFAIL
*     .. Local Arrays ..
      CHARACTER*51      REC(1)
*     .. External Functions ..
      INTEGER           P01ABF
      EXTERNAL          P01ABF
*     .. Intrinsic Functions ..
      INTRINSIC         SQRT
*     .. Executable Statements ..
      IF (X.GE.0.0D0) THEN
         Y = SQRT(X)
         IFAIL = 0
      ELSE
         WRITE (REC,99999) '** Attempt to take the square root of ', X
         IFAIL = P01ABF(IFAIL,1,SRNAME,1,REC)
      END IF
      RETURN
*
99999 FORMAT (1X,A,1P,D12.5)
      END
