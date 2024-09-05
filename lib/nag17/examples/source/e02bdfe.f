*     E02BDF Example Program Text.
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NC7MAX
      PARAMETER        (NC7MAX=100)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION DEFINT
      INTEGER          IFAIL, J, NCAP
*     .. Local Arrays ..
      DOUBLE PRECISION C(NC7MAX), K(NC7MAX)
*     .. External Subroutines ..
      EXTERNAL         E02BDF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'E02BDF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
   20 READ (NIN,*) NCAP
      IF (NCAP.GT.0 .AND. NCAP+7.LE.NC7MAX) THEN
         READ (NIN,*) (K(J),J=1,NCAP+7)
         READ (NIN,*) (C(J),J=1,NCAP+3)
         IFAIL = 0
*
         CALL E02BDF(NCAP+7,K,C,DEFINT,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,99999) 'Definite integral = ', DEFINT
         GO TO 20
      END IF
      STOP
*
99999 FORMAT (1X,A,D11.3)
      END
