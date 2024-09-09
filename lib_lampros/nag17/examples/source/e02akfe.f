*     E02AKF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NP1, LA
      PARAMETER        (NP1=7,LA=NP1)
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION P, X, XMAX, XMIN
      INTEGER          I, IFAIL, M
*     .. Local Arrays ..
      DOUBLE PRECISION A(LA)
*     .. External Subroutines ..
      EXTERNAL         E02AKF
*     .. Intrinsic Functions ..
      INTRINSIC        DBLE
*     .. Data statements ..
      DATA             XMIN, XMAX/-0.5D0, 2.5D0/
      DATA             (A(I),I=1,NP1)/2.53213D0, 1.13032D0, 0.27150D0,
     +                 0.04434D0, 0.00547D0, 0.00054D0, 0.00004D0/
*     .. Executable Statements ..
      WRITE (NOUT,*) 'E02AKF Example Program Results'
      WRITE (NOUT,*)
      WRITE (NOUT,*) '   I   Argument  Value of polynomial'
      M = 4
      DO 20 I = 1, M
         X = (XMIN*DBLE(M-I)+XMAX*DBLE(I-1))/DBLE(M-1)
         IFAIL = 0
*
         CALL E02AKF(NP1,XMIN,XMAX,A,1,LA,X,P,IFAIL)
*
         WRITE (NOUT,99999) I, X, P
   20 CONTINUE
      STOP
*
99999 FORMAT (1X,I4,F10.4,4X,F9.4)
      END
