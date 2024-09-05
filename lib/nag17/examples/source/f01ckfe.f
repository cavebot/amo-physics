*     F01CKF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          N, M, P, IZ
      PARAMETER        (N=2,M=3,P=2,IZ=1)
      INTEGER          NOUT
      PARAMETER        (NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J
*     .. Local Arrays ..
      DOUBLE PRECISION A(N,P), B(N,M), C(M,P), Z(IZ)
*     .. External Subroutines ..
      EXTERNAL         F01CKF
*     .. Intrinsic Functions ..
      INTRINSIC        DBLE
*     .. Executable Statements ..
      WRITE (NOUT,*) 'F01CKF Example Program Results'
      DO 20 I = 1, M
         B(1,I) = DBLE(I) - 1.0D0
         C(I,1) = B(1,I)
         B(2,I) = DBLE(I)
         C(I,2) = B(2,I)
   20 CONTINUE
      IFAIL = 0
*
      CALL F01CKF(A,B,C,N,P,M,Z,IZ,1,IFAIL)
*
      WRITE (NOUT,*)
      WRITE (NOUT,*) 'Matrix A'
      WRITE (NOUT,*)
      WRITE (NOUT,99999) ((A(I,J),J=1,P),I=1,N)
      STOP
*
99999 FORMAT (1X,2F7.1)
      END
