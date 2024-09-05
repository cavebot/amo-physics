      DOUBLE PRECISION FUNCTION X05BAF()
C     MARK 14 RELEASE. NAG COPYRIGHT 1989.
C     MARK 16 REVISED. IER-1051 (JUN 1993).
C
C     Returns the amount of CPU time used since some previous time,
C     in seconds. The previous time is arbitrary, but may be the time
C     that the job or the program started, for example. The difference
C     between two separate calls of this routine is then (approximately)
C     the CPU time used between the calls.
C
C     This routine is machine dependent. It must be modified by the
C     implementor to return the value described above.
C
C     The coding below works on a Silicon Graphics R3000 or R4000 under
C     IRIX 4.
C
C     .. Local Scalars ..
C     REAL                             TMP
C     .. Local Arrays ..
C     REAL                             TARRAY(2)
C     .. External Functions ..
C     REAL                             ETIME
C     EXTERNAL                         ETIME
C     .. Executable Statements ..
C     TMP = ETIME(TARRAY)
C     X05BAF = TARRAY(1)

C     The code below has been used AIX/xlf since Mk 16 (at least)
      INTEGER I1
      EXTERNAL MCLOCK
      I1 = MCLOCK()
      X05BAF = DBLE(I1)/100.0D0

C
C     The code below is present to prevent this routine from being
C     compiled unmodified. It must be replaced by code to return
C     the value described at the top of the routine.
C
C     .. Executable Statements ..
C      X05BAF = ******
      RETURN
      END
