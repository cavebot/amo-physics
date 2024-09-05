      SUBROUTINE Y90SFF(M,N,KL,KU,A,IA,B,IB)
C     MARK 15 RE-ISSUE. NAG COPYRIGHT 1991.
C-----------------------------------------------------------------------
C
C         =========================================
C         *  Y90SFF :  Transpose a Banded Matrix  *
C         =========================================
C
C-----------------------------------------------------------------------
C     .. Scalar Arguments ..
      INTEGER           IA, IB, KL, KU, M, N
C     .. Array Arguments ..
      DOUBLE PRECISION  A(IA,*), B(IB,*)
C     .. Local Scalars ..
      INTEGER           I, J
C     .. Intrinsic Functions ..
      INTRINSIC         MIN
C     .. Executable Statements ..
C-----------------------------------------------------------------------
C
C     Carry out transposition
C
C-----------------------------------------------------------------------
      DO 40 I = 1, KL + 1
         DO 20 J = 1, M - KL - 1 + I
            B(J,KL+KU+2-I) = A(J+KL+1-I,I)
   20    CONTINUE
   40 CONTINUE
C
      DO 80 I = KL + 2, KL + KU + 1
         DO 60 J = 1, MIN(N,N+KL+1-I)
            B(J+I-KL-1,KL+KU+2-I) = A(J,I)
   60    CONTINUE
   80 CONTINUE
C-----------------------------------------------------------------------
C
C     End of Y90SFF
C
C-----------------------------------------------------------------------
      RETURN
      END
