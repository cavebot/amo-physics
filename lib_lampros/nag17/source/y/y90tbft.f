      DOUBLE PRECISION FUNCTION Y90TBF(DIST,SEED)
C     MARK 15 RE-ISSUE. NAG COPYRIGHT 1991.
C
C  -- LAPACK test routine --
C     Courant Institute
C     September 30, 1988
C
C
C  Purpose
C  =======
C
C     This function returns the random number from the
C     distribution determined by DIST.
C
C  Arguments
C  =========
C
C  DIST  - INTEGER
C           On entry specifies the type of distribution to be used
C           to generate random matrix .
C           1=> UNIFORM( 0, 1 )
C           2=> UNIFORM( -1, 1 )
C           3=> NORMAL ( 0, 1 )
C           Unchanged on exit.
C
C
C
C
C
C     .. Parameters ..
      DOUBLE PRECISION                 ONE
      PARAMETER                        (ONE=1.0D0)
      DOUBLE PRECISION                 TWO
      PARAMETER                        (TWO=2.0D0)
C     .. Scalar Arguments ..
      INTEGER                          DIST
C     .. Array Arguments ..
      INTEGER                          SEED(4)
C     .. Local Scalars ..
      DOUBLE PRECISION                 DUMMY, PI, X, Y
C     .. External Functions ..
      DOUBLE PRECISION                 X01AAF, Y90TAF
      EXTERNAL                         X01AAF, Y90TAF
C     .. Intrinsic Functions ..
      INTRINSIC                        COS, LOG, SQRT
C     .. Executable Statements ..
C
      IF (DIST.EQ.1) THEN
         Y90TBF = Y90TAF(SEED)
      ELSE IF (DIST.EQ.2) THEN
         Y90TBF = TWO*Y90TAF(SEED) - ONE
      ELSE IF (DIST.EQ.3) THEN
         PI = X01AAF(DUMMY)
         X = SQRT(-TWO*LOG(Y90TAF(SEED)))
         Y = COS(TWO*PI*Y90TAF(SEED))
         Y90TBF = X*Y
      END IF
      RETURN
C
C     End of Y90TAFDM
      END
