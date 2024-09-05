      SUBROUTINE F04JGY(N,A,NRA,B,X,IFAIL)
C     MARK 8 RELEASE. NAG COPYRIGHT 1979.
C     MARK 11.5(F77) REVISED. (SEPT 1985.)
C     MARK 13 REVISED. USE OF MARK 12 X02 FUNCTIONS (APR 1988).
C     WRITTEN BY S. HAMMARLING, MIDDLESEX POLYTECHNIC (UPPERT)
C
C     ROUTINE F04JGY RETURNS THE SOLUTION X OF THE EQUATIONS
C
C     A*X = B ,
C
C     WHERE A IS AN N*N NON-SINGULAR UPPER TRIANGULAR MATRIX
C     AND B IS AN N ELEMENT VECTOR.
C
C     THE ROUTINE MAY BE CALLED WITH X=B.
C
C     ONLY THE UPPER TRIANGULAR PART OF A IS REFERENCED.
C
C     NRA MUST BE THE ACTUAL ROW DIMENSION OF A AS DECLARED IN
C     THE CALLING PROGRAM.
C
C     ON NORMAL RETURN IFAIL=0.
C
C     THE ROUTINE WILL TERMINATE EARLY, WITH IFAIL=I+1, IF THE
C     I(TH) DIAGONAL ELEMENT OF A IS TOO SMALL TO AVOID
C     OVERFLOW.
C     IF AN INPUT PARAMETER IS INCORRECTLY SUPPLIED THEN IFAIL
C     IS SET TO UNITY.
C
C     IF ON ENTRY IFAIL=0 THEN EARLY TERMINATION WILL BE HARD,
C     IF IFAIL=1 THEN EARLY TERMINATION WILL BE SOFT.
C
C     .. Scalar Arguments ..
      INTEGER           IFAIL, N, NRA
C     .. Array Arguments ..
      DOUBLE PRECISION  A(NRA,N), B(N), X(N)
C     .. Local Scalars ..
      DOUBLE PRECISION  SMALL, Y, Z
      INTEGER           I, J, JM1, K
      LOGICAL           UNDFLW
C     .. External Functions ..
      DOUBLE PRECISION  X02AMF
      LOGICAL           F04JGX, X02DAF
      EXTERNAL          X02AMF, F04JGX, X02DAF
C     .. External Subroutines ..
      EXTERNAL          F01QAW
C     .. Executable Statements ..
      IFAIL = 1
C
      IF (NRA.LT.N .OR. N.LT.1) GO TO 80
C
      SMALL = X02AMF()
      UNDFLW = X02DAF(0.0D0)
C
      DO 20 I = 1, N
         X(I) = B(I)
   20 CONTINUE
C
      J = N
      DO 40 K = 1, N
         Y = A(J,J)
         Z = X(J)
C
         IF (F04JGX(Z,Y,SMALL)) GO TO 60
C
         Z = Z/Y
         X(J) = Z
         IF (J.EQ.1) GO TO 40
         JM1 = J - 1
C
C        +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C        THE CALL TO F01QAW CAN BE REPLACED BY THE FOLLOWING IN-LINE
C        CODE, PROVIDED THAT NO PRECAUTIONS AGAINST UNDERFLOW
C        ARE REQUIRED
C
C        DO 30 I=1,JM1
C           X(I) = X(I) - Z*A(I,J)
C        30    CONTINUE
C
         CALL F01QAW(JM1,X,Z,A(1,J),UNDFLW)
C
C        +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
         J = JM1
   40 CONTINUE
C
      IFAIL = 0
      RETURN
C
   60 IFAIL = J + 1
   80 RETURN
      END
