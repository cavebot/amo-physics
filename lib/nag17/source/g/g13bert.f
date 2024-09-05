      SUBROUTINE G13BER(BETA,NBETA,MOP,MSN,NMS,MQAB,MQSG,NXSP,EPS,WD,
     *                  IDWD,KSFS)
C     MARK 11 RELEASE. NAG COPYRIGHT 1983.
C     MARK 11.5(F77) REVISED. (SEPT 1985.)
C
C     SUBROUTINE G13BER CHECKS VALIDITY OF EACH DELTA SET.
C     THE SUBSCRIPT OF THE FIRST OF THESE SETS TO SHOW
C     INVALIDITY IS OUTPUT IN KSFS.
C
C
C     LL AND LU ARE SUBSCRIPTS WHICH DEFINE THE T.F. OMEGA
C     AND DELTA AND THE ARIMA PARAMETERS IN THE BETA ARRAY.
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION  EPS
      INTEGER           IDWD, KSFS, NBETA, NMS, NXSP
C     .. Array Arguments ..
      DOUBLE PRECISION  BETA(NBETA), WD(IDWD)
      INTEGER           MOP(4), MQAB(8,NXSP), MQSG(8,NXSP), MSN(NMS)
C     .. Local Scalars ..
      DOUBLE PRECISION  PG
      INTEGER           I, J, K, KC, L, LL, LU, NDE, NXS
C     .. External Subroutines ..
      EXTERNAL          G13AEX
C     .. Executable Statements ..
      LL = MOP(3) + 1
      LU = MOP(4)
      KSFS = 0
      IF (NXSP.LE.1) GO TO 100
      NXS = NXSP - 1
C
C     PROCESS EACH INPUT SERIES IN TURN
C
      DO 80 I = 1, NXS
         NDE = MQSG(1,I)
         IF (NDE.LE.0) GO TO 80
C
C        TEST FOR INTERNAL ERROR
C
         DO 20 J = LL, LU
            IF (MQAB(1,I).EQ.MSN(J)) GO TO 40
   20    CONTINUE
C
C        PUT DELTAS INTO WORKING ARRAY WD
C
   40    DO 60 K = 1, NDE
            L = J + K - 2
            WD(K) = BETA(L)
   60    CONTINUE
C
C        TEST FOR VALIDITY OF THIS SET OF DELTAS
C
         CALL G13AEX(WD,NDE,EPS,PG,KC)
         IF (KC.GE.0) GO TO 80
         KSFS = I
         GO TO 100
   80 CONTINUE
  100 RETURN
      END
