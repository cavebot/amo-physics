*     G05FEF Example Program Text
*     Mark 15 Release. NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          N
      PARAMETER        (N=5)
*     .. Local Scalars ..
      INTEGER          IFAIL, J
*     .. Local Arrays ..
      DOUBLE PRECISION X(N)
*     .. External Subroutines ..
      EXTERNAL         G05CBF, G05FEF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G05FEF Example Program Results'
      WRITE (NOUT,*)
      IFAIL = 0
      CALL G05CBF(0)
      WRITE (NOUT,*) 'Beta Dist --- A=2.0, B=2.0'
*
      CALL G05FEF(2.0D0,2.0D0,N,X,IFAIL)
*
      WRITE (NOUT,99999) (X(J),J=1,N)
      STOP
*
99999 FORMAT (1X,F10.4)
      END
