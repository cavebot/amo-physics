*     G05EEF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          N
      DOUBLE PRECISION P
      INTEGER          NR
      PARAMETER        (N=50,P=0.5D0,NR=250)
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, IX
*     .. Local Arrays ..
      DOUBLE PRECISION R(NR)
*     .. External Functions ..
      INTEGER          G05EYF
      EXTERNAL         G05EYF
*     .. External Subroutines ..
      EXTERNAL         G05CBF, G05EEF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G05EEF Example Program Results'
      WRITE (NOUT,*)
      CALL G05CBF(0)
      IFAIL = 0
*
      CALL G05EEF(N,P,R,NR,IFAIL)
*
      DO 20 I = 1, 5
         IX = G05EYF(R,NR)
         WRITE (NOUT,99999) IX
   20 CONTINUE
      STOP
*
99999 FORMAT (1X,I5)
      END
