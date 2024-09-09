*     G13CBF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          KCMAX, NXMAX
      PARAMETER        (KCMAX=400,NXMAX=KCMAX/2)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION PW, PX
      INTEGER          I, IFAIL, KC, L, LG, MTX, MW, NG, NX
*     .. Local Arrays ..
      DOUBLE PRECISION STATS(4), XG(KCMAX), XH(NXMAX)
*     .. External Subroutines ..
      EXTERNAL         G13CBF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G13CBF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) NX
      IF (NX.GT.0 .AND. NX.LE.NXMAX) THEN
         READ (NIN,*) (XH(I),I=1,NX)
         MTX = 1
         PX = 0.2D0
         MW = NX
         PW = 0.5D0
         KC = 400
         L = 100
         LG = 1
   20    READ (NIN,*,END=60) MW
         IF (MW.GT.0 .AND. MW.LE.NX) THEN
            DO 40 I = 1, NX
               XG(I) = XH(I)
   40       CONTINUE
            IFAIL = 1
*
            CALL G13CBF(NX,MTX,PX,MW,PW,L,KC,LG,XG,NG,STATS,IFAIL)
*
            WRITE (NOUT,*)
            IF (MW.EQ.NX) THEN
               WRITE (NOUT,*) 'No smoothing'
            ELSE
               WRITE (NOUT,99999)
     +           'Frequency width of smoothing window = 1/', MW
            END IF
            WRITE (NOUT,*)
            IF (IFAIL.NE.0) THEN
               WRITE (NOUT,99999) 'G13CBF fails. IFAIL =', IFAIL
               WRITE (NOUT,*)
            END IF
            IF (IFAIL.EQ.0 .OR. IFAIL.GE.4) THEN
               WRITE (NOUT,99998) 'Degrees of freedom =', STATS(1),
     +           '      Bandwidth =', STATS(4)
               WRITE (NOUT,*)
               WRITE (NOUT,99997)
     +           '95 percent confidence limits -     Lower =', STATS(2),
     +           '  Upper =', STATS(3)
               WRITE (NOUT,*)
               WRITE (NOUT,*)
     +     '      Spectrum       Spectrum       Spectrum       Spectrum'
               WRITE (NOUT,*)
     +     '      estimate       estimate       estimate       estimate'
               WRITE (NOUT,99996) (I,XG(I),I=1,NG)
            END IF
            GO TO 20
         END IF
      END IF
   60 STOP
*
99999 FORMAT (1X,A,I3)
99998 FORMAT (1X,A,F4.1,A,F7.4)
99997 FORMAT (1X,A,F7.4,A,F7.4)
99996 FORMAT (1X,I4,F10.4,I5,F10.4,I5,F10.4,I5,F10.4)
      END
