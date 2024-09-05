*     G05CFF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION R
      INTEGER          I, IFAIL
*     .. Local Arrays ..
      DOUBLE PRECISION X(5), XA(4)
      INTEGER          IA(9)
*     .. External Functions ..
      DOUBLE PRECISION G05CAF
      EXTERNAL         G05CAF
*     .. External Subroutines ..
      EXTERNAL         G05CBF, G05CFF, G05CGF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G05CFF Example Program Results'
      WRITE (NOUT,*)
      CALL G05CBF(0)
      IFAIL = 0
      DO 20 I = 1, 5
         X(I) = G05CAF(R)
*
         IF (I.EQ.2) CALL G05CFF(IA,9,XA,4,IFAIL)
*
   20 CONTINUE
      WRITE (NOUT,99999) (X(I),I=1,5)
      DO 40 I = 1, 5
         X(I) = G05CAF(R)
*
         IF (I.EQ.2) CALL G05CGF(IA,9,XA,4,IFAIL)
*
   40 CONTINUE
      WRITE (NOUT,99999) (X(I),I=1,5)
      STOP
*
99999 FORMAT (1X,5F10.4)
      END
