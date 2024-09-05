      SUBROUTINE G13DST(ST,ITW,LP)
C     MARK 13 RELEASE. NAG COPYRIGHT 1988.
C     .. Scalar Arguments ..
      INTEGER           ITW, LP
C     .. Array Arguments ..
      CHARACTER*1       ST(80)
C     .. Local Scalars ..
      INTEGER           J
      CHARACTER*81      REC
C     .. External Subroutines ..
      EXTERNAL          X04BAF
C     .. Executable Statements ..
C
C     This subroutine prints a row of ITW *'s
C     On entry, ITW is always less than or equal to 80
C
      DO 20 J = 1, ITW
         ST(J) = '*'
   20 CONTINUE
      DO 40 J = ITW + 1, 80
         ST(J) = ' '
   40 CONTINUE
      WRITE (REC,FMT=99999) (ST(J),J=1,80)
      CALL X04BAF(LP,REC)
      RETURN
C
99999 FORMAT (' ',80A1)
      END
