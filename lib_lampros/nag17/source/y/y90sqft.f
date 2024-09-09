      SUBROUTINE Y90SQF(N,IROW,L,D,A)
C     MARK 15 RE-ISSUE. NAG COPYRIGHT 1991.
C-----------------------------------------------------------------------
C
C         =========================================
C         *  Y90SQF :  Product of a var.bandwidth  *
C         =========================================
C
C-----------------------------------------------------------------------
C     .. Parameters ..
      DOUBLE PRECISION  ZERO
      PARAMETER         (ZERO=0.0D0)
C     .. Scalar Arguments ..
      INTEGER           N
C     .. Array Arguments ..
      DOUBLE PRECISION  A(*), D(*), L(*)
      INTEGER           IROW(*)
C     .. Local Scalars ..
      DOUBLE PRECISION  TEMP
      INTEGER           I, J, K, K1
C     .. Intrinsic Functions ..
      INTRINSIC         MAX
C     .. Executable Statements ..
C-----------------------------------------------------------------------
C
C     Carry out product
C
C-----------------------------------------------------------------------
      DO 60 I = 1, N
         DO 40 J = I - IROW(I+1) + IROW(I) + 1, I
            TEMP = ZERO
            K1 = MAX((I-(IROW(I+1)-IROW(I))+1),(J-(IROW(J+1)-IROW(J))+1)
     *           )
            DO 20 K = K1, J
               TEMP = TEMP + L(IROW(I+1)-I+K-1)*D(K)*L(IROW(J+1)-J+K-1)
   20       CONTINUE
            A(IROW(I+1)-I+J-1) = TEMP
   40    CONTINUE
   60 CONTINUE
C-----------------------------------------------------------------------
C
C     End of Y90SQF
C
C-----------------------------------------------------------------------
      RETURN
      END
