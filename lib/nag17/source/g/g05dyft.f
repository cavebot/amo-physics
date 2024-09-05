      INTEGER FUNCTION G05DYF(M,N)
C     MARK 6 RELEASE  NAG COPYRIGHT 1976
C     MARK 7 REVISED IER-135 (DEC 1978)
C     MARK 11.5(F77) REVISED. (SEPT 1985.)
C     WRITTEN BY N.M.MACLAREN
C     UNIVERSITY OF CAMBRIDGE COMPUTER LABORATORY
C     THIS RETURNS AN INTEGER RESULT, UNIFORMLY DISTRIBUTED
C     BETWEEN M AND N INCLUSIVE.
C     THE CONTORTED PROGRAMMING IS TO GIVE CORRECT RESULTS AND TO
C     AVOID DIAGNOSTICS IN CASES WHERE:
C     1) ROUNDING CAUSES OVERFLOW OF THE RANGE (M,N).
C     2) INTEGER/REAL CONVERSION IS NOT EXACT.
C     3) INT TRUNCATES TOWARDS MINUS INFINITY.
C     .. Scalar Arguments ..
      INTEGER                 M, N
C     .. Local Scalars ..
      DOUBLE PRECISION        ONE, X, Y, Z
      INTEGER                 I, J, K
C     .. External Functions ..
      DOUBLE PRECISION        G05CAF
      EXTERNAL                G05CAF
C     .. Intrinsic Functions ..
      INTRINSIC               ABS, MAX, MIN, DBLE, INT
C     .. Data statements ..
      DATA                    ONE/1.0D0/
C     .. Executable Statements ..
      Y = DBLE(M)
      Z = DBLE(N)
      X = MIN(Y,Z) + (ABS(Z-Y)+ONE)*G05CAF(X)
      I = INT(X)
      IF (DBLE(I).GT.X) I = I - 1
C     ON A MACHINE WHICH OVERFLOWS WHEN COMPARING THE LARGEST (MOST
C     POSITIVE) AND SMALLEST (MOST NEGATIVE) INTEGERS, THE
C     FOLLOWING STATEMENTS SHOULD BE INSERTED HERE:
C     IF (M) 50, 140, 55
C     50 IF (N) 140, 60, 60
C     55 IF (N) 80, 80, 140
C     60 J = M
C     K = N
C     GO TO 100
C     80 J = N
C     K = M
C     100 IF (I.GT.0) GO TO 120
C     G05DYF = MAX0(I,J)
C     RETURN
C     120 G05DYF = MIN0(I,K)
C     RETURN
C     140 CONTINUE
      IF (M.GT.N) GO TO 20
      J = M
      K = N
      GO TO 40
   20 J = N
      K = M
   40 G05DYF = MIN(MAX(I,J),K)
      RETURN
      END
