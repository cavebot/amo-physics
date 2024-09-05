*     G05EXF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NP, NR
      PARAMETER        (NP=10,NR=19)
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, IX
*     .. Local Arrays ..
      DOUBLE PRECISION P(NP), R(NR)
*     .. External Functions ..
      INTEGER          G05EYF
      EXTERNAL         G05EYF
*     .. External Subroutines ..
      EXTERNAL         G05CBF, G05EXF
*     .. Data statements ..
      DATA             P/0.0D0, 0.1D0, 0.2D0, 0.4D0, 0.5D0, 0.6D0,
     +                 0.8D0, 0.9D0, 1.0D0, 1.0D0/
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G05EXF Example Program Results'
      WRITE (NOUT,*)
      CALL G05CBF(0)
      IFAIL = 0
*
      CALL G05EXF(P,NP,0,.TRUE.,R,NR,IFAIL)
*
      DO 20 I = 1, 5
         IX = G05EYF(R,NR)
         WRITE (NOUT,99999) IX
   20 CONTINUE
      STOP
*
99999 FORMAT (1X,I5)
      END
