*     D01BBF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          N
      PARAMETER        (N=6)
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION A, B
      INTEGER          IFAIL, ITYPE, J
*     .. Local Arrays ..
      DOUBLE PRECISION ABSCIS(N), WEIGHT(N)
*     .. External Subroutines ..
      EXTERNAL         D01BAX, D01BBF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D01BBF Example Program Results'
      A = 0.0D0
      B = 1.0D0
      ITYPE = 1
      IFAIL = 0
*
      CALL D01BBF(D01BAX,A,B,ITYPE,N,WEIGHT,ABSCIS,IFAIL)
*
      WRITE (NOUT,*)
      WRITE (NOUT,99998) 'Laguerre formula,', N, ' points'
      WRITE (NOUT,*)
      WRITE (NOUT,*) '    Abscissae        Weights'
      WRITE (NOUT,*)
      WRITE (NOUT,99999) (ABSCIS(J),WEIGHT(J),J=1,N)
      STOP
*
99999 FORMAT (1X,2D15.6)
99998 FORMAT (1X,A,I3,A)
      END
