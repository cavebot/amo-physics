      SUBROUTINE X05AAF(ITIME)
C     MARK 14 RELEASE. NAG COPYRIGHT 1989.
C
C     Returns the current date and time in the integer array ITIME.
C     On exit, ITIME should contain the following values:
C       ITIME(1) : the current year.
C       ITIME(2) : the current month, in the range 1 - 12.
C       ITIME(3) : the current day, in the range 1 - 31.
C       ITIME(4) : the current hour, in the range 0 - 23.
C       ITIME(5) : the current minute, in the range 0 - 59.
C       ITIME(6) : the current second, in the range 0 - 59.
C       ITIME(7) : the current millisecond, in the range 0 - 999.
C
C     This routine is machine dependent. It must be modified by the
C     implementor to return the values described above. If it is not
C     possible on a particular machine to return a correct value for
C     any or all of the above elements, those elements should be set
C     to zero.
C
C     The following sample code works on a Silicon Graphics R3000 or
C     R4000 under IRIX 4
C     .. Array Arguments ..
      INTEGER           ITIME(7)
C     .. Local Scalars ..
C     INTEGER           ITIME2(9)
C     .. External Functions ..
C     INTEGER           TIME
C     EXTERNAL          TIME
C     .. External Subroutines ..
C     EXTERNAL          LTIME
C     .. Executable Statements ..
C
C     CALL LTIME(TIME(),ITIME2)
C     ITIME(1) = ITIME2(6) + 1900
C     ITIME(2) = ITIME2(5) + 1
C     ITIME(3) = ITIME2(4)
C     ITIME(4) = ITIME2(3)
C     ITIME(5) = ITIME2(2)
C     ITIME(6) = ITIME2(1)
C     ITIME(7) = 0

C     The code below has been used for AIX/xlf since Mk 16 (at least)
      INTEGER           ITIME2(9),UTIMES
      CALL GMTIME_(UTIMES,ITIME2)
      ITIME(1) = ITIME2(6)+1900
      ITIME(2) = ITIME2(5)+1
      ITIME(3) = ITIME2(4)
      ITIME(4) = ITIME2(3)
      ITIME(5) = ITIME2(2)
      ITIME(6) = ITIME2(1)
      ITIME(7) = UTIMES/1000

C
C     The code below is present to prevent this routine from being
C     compiled unmodified. It must be replaced by code to return
C     the values described at the top of the routine.
C
C      ITIME(1) = *******
C      ITIME(2) = *******
C      ITIME(3) = *******
C      ITIME(4) = *******
C      ITIME(5) = *******
C      ITIME(6) = *******
C      ITIME(7) = *******
C
      RETURN
      END
