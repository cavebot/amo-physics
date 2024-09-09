      SUBROUTINE A00AAZ(MSG)
C     MARK 17 RELEASE. NAG COPYRIGHT 1995.
C
C     Returns information about the particular implementation of the
C     NAG Fortran Library in use.
C
C     **************************************************************
C
C     Implementors must insert the correct details for each
C     distinct implementation.
C
C     **************************************************************
C
C     .. Array Arguments ..
      CHARACTER*80      MSG(20)
C     .. Executable Statements ..
      MSG(1) = ' *** Start of NAG Library implementation details ***'
      MSG(2) = ' '
      MSG(3) = ' Implementation title: IBM RISC System/6000 AIX'
      MSG(4) = '            Precision: Double'
      MSG(5) = '         Product Code: FLIB617DA'
      MSG(6) = '                 Mark: 17A'
      MSG(7) = ' '
      MSG(8) = ' Created using:'
      MSG(9) = '     hardware -   IBM RISC System 6000'
      MSG(10) = '     op. sys. -   AIX v3.2'
      MSG(11) = '     compiler -   xlf v02.03.0000.0023'
      MSG(12) = ' '
      MSG(13) = ' '
      MSG(14) = ' '
      MSG(15) = ' Applicable to:'
      MSG(16) = '     hardware -   IBM RISC System/6000'
      MSG(17) = '     op. sys. -   AIX v3.2'
      MSG(18) = '     compiler -   XL FORTRAN Compiler/6000 v2'
      MSG(19) = ' '
      MSG(20) = ' *** End of NAG Library implementation details ***'
      RETURN
      END
