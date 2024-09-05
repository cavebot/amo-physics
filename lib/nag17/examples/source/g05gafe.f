*     G05GAF Example Program Text
*     Mark 16 Release. NAG Copyright 1992.
*     .. Parameters ..
      INTEGER          N, M, LDA
      PARAMETER        (N=4,M=4,LDA=10)
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,N), WK(2*N)
*     .. External Subroutines ..
      EXTERNAL         G05CBF, G05GAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G05GAF Example Program Results'
      WRITE (NOUT,*)
*
      CALL G05CBF(0)
*
      IFAIL = 0
*
      CALL G05GAF('Right','Initialize',M,N,A,LDA,WK,IFAIL)
*
      DO 20 I = 1, M
         WRITE (NOUT,99999) (A(I,J),J=1,N)
   20 CONTINUE
      STOP
*
99999 FORMAT (1X,4F9.3)
      END
