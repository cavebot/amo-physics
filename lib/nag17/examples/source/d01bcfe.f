*     D01BCF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          N
      PARAMETER        (N=7)
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION A, B, C, D
      INTEGER          IFAIL, ITYPE, J
*     .. Local Arrays ..
      DOUBLE PRECISION ABSCIS(N), WEIGHT(N)
*     .. External Subroutines ..
      EXTERNAL         D01BCF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'D01BCF Example Program Results'
      A = 0.0D0
      B = 1.0D0
      C = 0.0D0
      D = 0.0D0
      ITYPE = -3
      IFAIL = 0
*
      CALL D01BCF(ITYPE,A,B,C,D,N,WEIGHT,ABSCIS,IFAIL)
*
      WRITE (NOUT,*)
      WRITE (NOUT,99999) 'Laguerre formula,', N, ' points'
      WRITE (NOUT,*)
      WRITE (NOUT,*) '      Abscissae             Weights'
      WRITE (NOUT,*)
      WRITE (NOUT,99998) (ABSCIS(J),WEIGHT(J),J=1,N)
      STOP
*
99999 FORMAT (1X,A,I3,A)
99998 FORMAT (1X,D15.5,5X,D15.5)
      END
