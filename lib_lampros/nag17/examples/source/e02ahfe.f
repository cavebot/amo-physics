*     E02AHF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NP1, LA, LADIF
      PARAMETER        (NP1=7,LA=NP1,LADIF=NP1)
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION DERIV, DERIV2, PATM1, X, XMAX, XMIN
      INTEGER          I, IFAIL, M
*     .. Local Arrays ..
      DOUBLE PRECISION A(LA), ADIF(LADIF), ADIF2(LADIF)
*     .. External Subroutines ..
      EXTERNAL         E02AHF, E02AKF
*     .. Intrinsic Functions ..
      INTRINSIC        DBLE
*     .. Data statements ..
      DATA             XMIN, XMAX/-0.5D0, 2.5D0/
      DATA             (A(I),I=1,NP1)/2.53213D0, 1.13032D0, 0.27150D0,
     +                 0.04434D0, 0.00547D0, 0.00054D0, 0.00004D0/
*     .. Executable Statements ..
      WRITE (NOUT,*) 'E02AHF Example Program Results'
      IFAIL = 0
*
      CALL E02AHF(NP1,XMIN,XMAX,A,1,LA,PATM1,ADIF,1,LADIF,IFAIL)
      CALL E02AHF(NP1-1,XMIN,XMAX,ADIF,1,LADIF,PATM1,ADIF2,1,LADIF,
     +            IFAIL)
*
      M = 4
      WRITE (NOUT,*)
      WRITE (NOUT,*) '   I  Argument    1st deriv    2nd deriv'
      DO 20 I = 1, M
         X = (XMIN*DBLE(M-I)+XMAX*DBLE(I-1))/DBLE(M-1)
*
         CALL E02AKF(NP1-1,XMIN,XMAX,ADIF,1,LADIF,X,DERIV,IFAIL)
         CALL E02AKF(NP1-2,XMIN,XMAX,ADIF2,1,LADIF,X,DERIV2,IFAIL)
*
         WRITE (NOUT,99999) I, X, DERIV, DERIV2
   20 CONTINUE
      STOP
*
99999 FORMAT (1X,I4,F9.4,2(4X,F9.4))
      END
