*     G05FSF Example Program Text
*     Mark 16 Release. NAG Copyright 1992.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          N
      PARAMETER        (N=10)
*     .. Local Scalars ..
      INTEGER          I, IFAIL
*     .. Local Arrays ..
      DOUBLE PRECISION X(N)
*     .. External Subroutines ..
      EXTERNAL         G05CBF, G05FSF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G05FSF Example Program Results'
      WRITE (NOUT,*)
      IFAIL = 0
*
      CALL G05CBF(0)
*
      WRITE (NOUT,*) 'Von Mises Dist --- VK = 2.0'
*
      CALL G05FSF(2.0D0,N,X,IFAIL)
*
      WRITE (NOUT,99999) (X(I),I=1,N)
      STOP
*
99999 FORMAT (1X,F10.4)
      END
