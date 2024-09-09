      SUBROUTINE Y90SMF(VMAN,VEXP,SCALE)
C     MARK 15 RE-ISSUE. NAG COPYRIGHT 1991.
C-----------------------------------------------------------------------
C
C         ==============================
C         *  Y90SMF :  Binary Scaling  *
C         ==============================
C
C-----------------------------------------------------------------------
C     .. Parameters ..
      DOUBLE PRECISION  ZERO, ONE
      PARAMETER         (ZERO=0.0D0,ONE=1.0D0)
C     .. Scalar Arguments ..
      DOUBLE PRECISION  VMAN
      INTEGER           SCALE, VEXP
C     .. Local Scalars ..
      DOUBLE PRECISION  LOWB, RSCALE
      LOGICAL           LOOP
C     .. Intrinsic Functions ..
      INTRINSIC         ABS, DBLE
C     .. Executable Statements ..
C-----------------------------------------------------------------------
C
C     Processing
C
C-----------------------------------------------------------------------
      RSCALE = DBLE(2**SCALE)
      LOWB = ONE/RSCALE
      IF (VMAN.NE.ZERO) THEN
   20    CONTINUE
         IF (ABS(VMAN).GE.ONE) THEN
            VMAN = VMAN*LOWB
            VEXP = VEXP + SCALE
            LOOP = .TRUE.
         ELSE
            LOOP = .FALSE.
         END IF
         IF (LOOP) GO TO 20
C
   40    CONTINUE
         IF (ABS(VMAN).LT.LOWB) THEN
            VMAN = VMAN*RSCALE
            VEXP = VEXP - SCALE
            LOOP = .TRUE.
         ELSE
            LOOP = .FALSE.
         END IF
         IF (LOOP) GO TO 40
C
      ELSE
         VMAN = ZERO
         VEXP = 0
      END IF
C-----------------------------------------------------------------------
C
C     End of Y90SMF
C
C-----------------------------------------------------------------------
      RETURN
      END
