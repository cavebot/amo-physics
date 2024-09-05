*     G05FBF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
      INTEGER          N
      PARAMETER        (N=5)
*     .. Local Scalars ..
      INTEGER          I
*     .. Local Arrays ..
      DOUBLE PRECISION X(N)
*     .. External Subroutines ..
      EXTERNAL         G05CBF, G05FBF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G05FBF Example Program Results'
      CALL G05CBF(0)
*
      CALL G05FBF(2.0D0,N,X)
*
      WRITE (NOUT,99999) (X(I),I=1,N)
      STOP
*
99999 FORMAT (1X,F10.4)
      END
