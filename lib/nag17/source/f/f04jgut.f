      DOUBLE PRECISION FUNCTION F04JGU(SCALE,SUMSQ,BIG)
C     MARK 8 RELEASE. NAG COPYRIGHT 1979.
C     MARK 11.5(F77) REVISED. (SEPT 1985.)
C     MARK 13 REVISED. USE OF MARK 12 X02 FUNCTIONS (APR 1988).
C     WRITTEN BY S. HAMMARLING, MIDDLESEX POLYTECHNIC (TWONRM)
C
C     F04JGU RETURNS THE VALUE
C
C     F04JGU = SCALE*SQRT(SUMSQ) .
C
C     SCALE IS ASSUMED TO BE NON-NEGATIVE AND SUMSQ IS ASSUMED
C     TO BE AT LEAST UNITY.
C
C     BIG MUST BE GIVEN BY
C
C     BIG = 1.0/X02AMF ,
C
C     WHERE X02AMF IS THE SMALL REAL VALUE RETURNED FROM
C     ROUTINE X02AMF.
C
C     F04JGU IS USED IN CONJUNCTION WITH F04JGT BY VARIOUS
C     EUCLIDEAN NORM ROUTINES.
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION                 BIG, SCALE, SUMSQ
C     .. Intrinsic Functions ..
      INTRINSIC                        SQRT
C     .. Executable Statements ..
      IF (SCALE.GE.BIG/SUMSQ) GO TO 20
      F04JGU = SCALE*SQRT(SUMSQ)
      RETURN
C
   20 F04JGU = BIG
      RETURN
      END
