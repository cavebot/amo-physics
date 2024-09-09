      DOUBLE PRECISION FUNCTION Y90SRF(NORM,N,IROW,A,WORK)
C     MARK 15 RE-ISSUE. NAG COPYRIGHT 1991.
C-----------------------------------------------------------------------
C
C         ===================================================
C         *  Y90SRF :  Norm of a variable bandwidth matrix  *
C         ===================================================
C
C-----------------------------------------------------------------------
C     .. Parameters ..
      DOUBLE PRECISION                 ZERO, ONE, TWO
      PARAMETER                        (ZERO=0.0D0,ONE=1.0D0,TWO=2.0D0)
C     .. Scalar Arguments ..
      INTEGER                          N
      CHARACTER*(*)                    NORM
C     .. Array Arguments ..
      DOUBLE PRECISION                 A(*), WORK(*)
      INTEGER                          IROW(*)
C     .. Local Scalars ..
      DOUBLE PRECISION                 SCALE, SQTWO, SUM, VALUE
      INTEGER                          I, J, K
C     .. External Functions ..
      DOUBLE PRECISION                 F06BMF
      LOGICAL                          Y90WAF
      EXTERNAL                         F06BMF, Y90WAF
C     .. External Subroutines ..
      EXTERNAL                         F06FJF
C     .. Intrinsic Functions ..
      INTRINSIC                        ABS, MAX, SQRT
C     .. Executable Statements ..
C-----------------------------------------------------------------------
C
C     Calculate Frobenius F-Norm of A
C
C-----------------------------------------------------------------------
      IF (Y90WAF(NORM,'F')) THEN
         SCALE = ZERO
         SUM = ONE
         SQTWO = ONE/SQRT(TWO)
C
         DO 20 I = 1, N
            WORK(I) = A(IROW(I+1)-1)
            A(IROW(I+1)-1) = A(IROW(I+1)-1)*SQTWO
   20    CONTINUE
C
         CALL F06FJF(IROW(N+1)-1,A(1),1,SCALE,SUM)
C
         SUM = TWO*SUM
         VALUE = F06BMF(SCALE,SUM)
C
         DO 40 I = 1, N
            A(IROW(I+1)-1) = WORK(I)
   40    CONTINUE
C-----------------------------------------------------------------------
C
C     Calculate 1-Norm of A
C
C-----------------------------------------------------------------------
      ELSE IF (Y90WAF(NORM,'1')) THEN
         DO 60 I = 1, N
            WORK(I) = ZERO
   60    CONTINUE
C
         DO 100 I = 1, N
            DO 80 J = IROW(I), IROW(I+1) - 2
               K = I + J + 1 - IROW(I+1)
               WORK(K) = WORK(K) + ABS(A(J))
               WORK(I) = WORK(I) + ABS(A(J))
   80       CONTINUE
            WORK(I) = WORK(I) + ABS(A(IROW(I+1)-1))
  100    CONTINUE
C
         VALUE = ZERO
         DO 120 I = 1, N
            VALUE = MAX(VALUE,ABS(WORK(I)))
  120    CONTINUE
C-----------------------------------------------------------------------
C
C     Calculate  max(abs(A(i,j)))
C
C-----------------------------------------------------------------------
      ELSE
C
         VALUE = ZERO
         DO 140 I = 1, IROW(N+1) - 1
            VALUE = MAX(VALUE,ABS(A(I)))
  140    CONTINUE
C
      END IF
C
      Y90SRF = VALUE
C-----------------------------------------------------------------------
C
C     End of Y90SRF
C
C-----------------------------------------------------------------------
      RETURN
      END
