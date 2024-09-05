      LOGICAL FUNCTION Y90WAF(CA,CB)
C     MARK 15 RE-ISSUE. NAG COPYRIGHT 1991.
C-----------------------------------------------------------------------
C
C         ================================================
C         *  Y90WAF :  Test two characters for equality  *
C         ================================================
C
C
C  Purpose
C  =======
C
C  Y90WAF  tests if CA is the same letter as CB regardless of case.
C  CB is assumed to be an upper case letter. Y90WAF returns .TRUE. if
C  CA is either the same as CB or the equivalent lower case letter.
C
C  N.B. This version of the routine is only correct for ASCII code.
C       Installers must modify the routine for other character-codes.
C
C       For EBCDIC systems the constant IOFF must be changed to -64.
C       For CDC systems using 6-12 bit representations, the system-
C       specific code in comments must be activated.
C
C  Parameters
C  ==========
C
C  CA     - CHARACTER*1
C  CB     - CHARACTER*1
C           On entry, CA and CB specify characters to be compared.
C           Unchanged on exit.
C
C
C-----------------------------------------------------------------------
C     .. Scalar Arguments ..
      CHARACTER*1             CA, CB
C     .. Local Scalars ..
      INTEGER                 IOFF
C     .. Intrinsic Functions ..
      INTRINSIC               ABS, ICHAR
C     .. Save statement ..
      SAVE                    IOFF
C     .. Data statements ..
      DATA                    IOFF/0/
C     .. Executable Statements ..
C-----------------------------------------------------------------------
C
C     Test the characters
C
C-----------------------------------------------------------------------
      IF (IOFF.LE.0) THEN
         IOFF = ICHAR('a') - ICHAR('A')
      END IF
      Y90WAF = (CA.EQ.CB) .OR. (ABS(ICHAR(CA)-ICHAR(CB)).EQ.IOFF)
C-----------------------------------------------------------------------
C
C     End of Y90WAF
C
C-----------------------------------------------------------------------
      RETURN
      END
