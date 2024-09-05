*     G13CEF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NGMAX
      PARAMETER        (NGMAX=9)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION T
      INTEGER          I, IFAIL, J, NG
*     .. Local Arrays ..
      DOUBLE PRECISION CA(NGMAX), CALW(NGMAX), CAUP(NGMAX), SC(NGMAX),
     +                 SCLW(NGMAX), SCUP(NGMAX), STATS(4), XG(NGMAX),
     +                 XYIG(NGMAX), XYRG(NGMAX), YG(NGMAX)
*     .. External Subroutines ..
      EXTERNAL         G13CEF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G13CEF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) NG
      READ (NIN,*) (STATS(I),I=1,4)
      READ (NIN,*) (XG(I),YG(I),XYRG(I),XYIG(I),I=1,NG)
      IFAIL = 1
*
      CALL G13CEF(XG,YG,XYRG,XYIG,NG,STATS,CA,CALW,CAUP,T,SC,SCLW,SCUP,
     +            IFAIL)
*
      WRITE (NOUT,*)
      IF (IFAIL.NE.0) THEN
         WRITE (NOUT,99999) 'G13CEF fails. IFAIL =', IFAIL
         WRITE (NOUT,*)
      END IF
      IF (IFAIL.NE.1) THEN
         WRITE (NOUT,*) '      Cross amplitude spectrum'
         WRITE (NOUT,*)
         WRITE (NOUT,*) '                    Lower     Upper'
         WRITE (NOUT,*) '          Value     bound     bound'
         DO 20 J = 1, NG
            WRITE (NOUT,99998) J - 1, CA(J), CALW(J), CAUP(J)
   20    CONTINUE
         WRITE (NOUT,*)
         WRITE (NOUT,99997) 'Squared coherency test statistic =', T
         WRITE (NOUT,*)
         WRITE (NOUT,*) '         Squared coherency'
         WRITE (NOUT,*)
         WRITE (NOUT,*) '                    Lower     Upper'
         WRITE (NOUT,*) '          Value     bound     bound'
         DO 40 J = 1, NG
            WRITE (NOUT,99998) J - 1, SC(J), SCLW(J), SCUP(J)
   40    CONTINUE
      END IF
      STOP
*
99999 FORMAT (1X,A,I3)
99998 FORMAT (1X,I5,3F10.4)
99997 FORMAT (1X,A,F12.4)
      END
