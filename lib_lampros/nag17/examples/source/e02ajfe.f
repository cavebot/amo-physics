*     E02AJF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NP1, LA, LAINT
      PARAMETER        (NP1=7,LA=NP1,LAINT=NP1+1)
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION RA, RB, RESULT, XA, XB, XMAX, XMIN
      INTEGER          I, IFAIL
*     .. Local Arrays ..
      DOUBLE PRECISION A(LA), AINT(LAINT)
*     .. External Subroutines ..
      EXTERNAL         E02AJF, E02AKF
*     .. Data statements ..
      DATA             XMIN, XMAX/-0.5D0, 2.5D0/
      DATA             (A(I),I=1,NP1)/2.53213D0, 1.13032D0, 0.27150D0,
     +                 0.04434D0, 0.00547D0, 0.00054D0, 0.00004D0/
*     .. Executable Statements ..
      WRITE (NOUT,*) 'E02AJF Example Program Results'
      IFAIL = 0
*
      CALL E02AJF(NP1,XMIN,XMAX,A,1,LA,0.0D0,AINT,1,LAINT,IFAIL)
*
      XA = 0.0D0
      XB = 2.0D0
*
      CALL E02AKF(NP1+1,XMIN,XMAX,AINT,1,LAINT,XA,RA,IFAIL)
      CALL E02AKF(NP1+1,XMIN,XMAX,AINT,1,LAINT,XB,RB,IFAIL)
*
      RESULT = RB - RA
      WRITE (NOUT,*)
      WRITE (NOUT,99999) 'Value of definite integral is ', RESULT
      STOP
*
99999 FORMAT (1X,A,F10.4)
      END
