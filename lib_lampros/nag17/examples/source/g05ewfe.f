*     G05EWF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NA, NB, NR
      PARAMETER        (NA=2,NB=1,NR=NA+NB+4+NA)
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION VAR, X
      INTEGER          I, IFAIL
*     .. Local Arrays ..
      DOUBLE PRECISION A(NA), B(NB), R(NR)
*     .. External Functions ..
      DOUBLE PRECISION G05EWF
      EXTERNAL         G05EWF
*     .. External Subroutines ..
      EXTERNAL         G05CBF, G05EGF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G05EWF Example Program Results'
      WRITE (NOUT,*)
      CALL G05CBF(0)
      A(1) = 0.4D0
      A(2) = 0.2D0
      B(1) = 1.0D0
      IFAIL = 0
*
      CALL G05EGF(0.0D0,A,NA,B,NB,R,NR,VAR,IFAIL)
*
      DO 20 I = 1, 10
         IFAIL = 0
         X = G05EWF(R,NR,IFAIL)
         WRITE (NOUT,99999) X
   20 CONTINUE
      STOP
*
99999 FORMAT (1X,F12.4)
      END
