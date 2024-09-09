*     F01CRF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          M, N, MN, LMOVE
      PARAMETER        (M=3,N=7,MN=M*N,LMOVE=(M+N)/2)
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, IFAIL
*     .. Local Arrays ..
      DOUBLE PRECISION A(MN)
      INTEGER          MOVE(LMOVE)
*     .. External Subroutines ..
      EXTERNAL         F01CRF
*     .. Intrinsic Functions ..
      INTRINSIC        DBLE
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F01CRF Example Program Results'
      DO 20 I = 1, MN
         A(I) = DBLE(I)
   20 CONTINUE
      IFAIL = 0
*
      CALL F01CRF(A,M,N,MN,MOVE,LMOVE,IFAIL)
*
      WRITE (NOUT,*)
      WRITE (NOUT,99999) (A(I),I=1,MN)
      STOP
*
99999 FORMAT (1X,7F7.1)
      END
