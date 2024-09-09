*     G13CFF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NGMAX
      PARAMETER        (NGMAX=9)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J, NG
*     .. Local Arrays ..
      DOUBLE PRECISION GN(NGMAX), GNLW(NGMAX), GNUP(NGMAX), PH(NGMAX),
     +                 PHLW(NGMAX), PHUP(NGMAX), STATS(4), XG(NGMAX),
     +                 XYIG(NGMAX), XYRG(NGMAX), YG(NGMAX)
*     .. External Subroutines ..
      EXTERNAL         G13CFF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G13CFF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) NG
      IF (NG.GT.0 .AND. NG.LE.NGMAX) THEN
         READ (NIN,*) (STATS(I),I=1,4)
         READ (NIN,*) (XG(I),YG(I),XYRG(I),XYIG(I),I=1,NG)
         IFAIL = 1
*
         CALL G13CFF(XG,YG,XYRG,XYIG,NG,STATS,GN,GNLW,GNUP,PH,PHLW,PHUP,
     +               IFAIL)
*
         WRITE (NOUT,*)
         IF (IFAIL.NE.0) THEN
            WRITE (NOUT,99999) 'G13CFF fails. IFAIL =', IFAIL
            WRITE (NOUT,*)
         END IF
         IF (IFAIL.NE.1) THEN
            WRITE (NOUT,*) '              The gain'
            WRITE (NOUT,*)
            WRITE (NOUT,*) '                    Lower     Upper'
            WRITE (NOUT,*) '          Value     bound     bound'
            DO 20 J = 1, NG
               WRITE (NOUT,99998) J - 1, GN(J), GNLW(J), GNUP(J)
   20       CONTINUE
            WRITE (NOUT,*)
            WRITE (NOUT,*) '             The phase'
            WRITE (NOUT,*)
            WRITE (NOUT,*) '                    Lower     Upper'
            WRITE (NOUT,*) '          Value     bound     bound'
            DO 40 J = 1, NG
               WRITE (NOUT,99998) J - 1, PH(J), PHLW(J), PHUP(J)
   40       CONTINUE
         END IF
      END IF
      STOP
*
99999 FORMAT (1X,A,I3)
99998 FORMAT (1X,I5,3F10.4)
      END
