      DOUBLE PRECISION FUNCTION Y90TAF(SEED)
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
C     Generate a random number uniformly distributed between 0. and 1.
C     A linear congruential sequence is used. This code is machine
C     independent, provided 12 bit integers can be added and multiplied
C     correctly.
C
C
C
C
C
C     .. Parameters ..
      INTEGER                          M1
      PARAMETER                        (M1=502)
      INTEGER                          M2
      PARAMETER                        (M2=1521)
      INTEGER                          M3
      PARAMETER                        (M3=4071)
      INTEGER                          M4
      PARAMETER                        (M4=2107)
      DOUBLE PRECISION                 ONE
      PARAMETER                        (ONE=1.0D0)
      DOUBLE PRECISION                 T12
      PARAMETER                        (T12=4096.0D0)
C     .. Array Arguments ..
      INTEGER                          SEED(4)
C     .. Local Scalars ..
      DOUBLE PRECISION                 R
      INTEGER                          I1, I2, I3, I4
C     .. Intrinsic Functions ..
      INTRINSIC                        MOD, DBLE
C     .. Executable Statements ..
C
      I1 = SEED(1)*M4 + SEED(2)*M3 + SEED(3)*M2 + SEED(4)*M1
      I2 = SEED(2)*M4 + SEED(3)*M3 + SEED(4)*M2
      I3 = SEED(3)*M4 + SEED(4)*M3
      I4 = SEED(4)*M4
      SEED(4) = MOD(I4,4096)
      I3 = I3 + I4/4096
      SEED(3) = MOD(I3,4096)
      I2 = I2 + I3/4096
      SEED(2) = MOD(I2,4096)
      SEED(1) = MOD(I1+I2/4096,4096)
      R = ONE/T12
      Y90TAF = R*(DBLE(SEED(1))+R*(DBLE(SEED(2))+R*(DBLE(SEED(3))
     *         +R*(DBLE(SEED(4))))))
      RETURN
C
C     End of Y90TAF
      END
