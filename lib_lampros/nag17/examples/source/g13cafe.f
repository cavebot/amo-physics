*     G13CAF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NXG, NCMAX
      PARAMETER        (NXG=500,NCMAX=200)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION PX
      INTEGER          I, IC, IFAIL, IW, KC, L, LG, MTX, MW, NC, NG, NX
*     .. Local Arrays ..
      DOUBLE PRECISION C(NCMAX), STATS(4), XG(NXG)
*     .. External Subroutines ..
      EXTERNAL         G13CAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G13CAF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) NX, NC
      IF (NX.GT.0 .AND. NX.LE.NXG .AND. NC.GT.0 .AND. NC.LE.NCMAX) THEN
         READ (NIN,*) (XG(I),I=1,NX)
         MTX = 1
         PX = 0.1D0
         IW = 4
         MW = 100
         IC = 0
         KC = 360
         L = 200
         LG = 0
         IFAIL = 1
*
         CALL G13CAF(NX,MTX,PX,IW,MW,IC,NC,C,KC,L,LG,NXG,XG,NG,STATS,
     +               IFAIL)
*
         WRITE (NOUT,*)
         IF (IFAIL.NE.0) THEN
            WRITE (NOUT,99999) 'G13CAF fails. IFAIL =', IFAIL
            WRITE (NOUT,*)
         END IF
         IF (IFAIL.EQ.0 .OR. IFAIL.GE.4) THEN
            WRITE (NOUT,*) 'Covariances'
            WRITE (NOUT,99998) (C(I),I=1,NC)
            WRITE (NOUT,*)
            WRITE (NOUT,99997) 'Degrees of freedom =', STATS(1),
     +        '      Bandwidth =', STATS(4)
            WRITE (NOUT,*)
            WRITE (NOUT,99996)
     +        '95 percent confidence limits -     Lower =', STATS(2),
     +        '  Upper =', STATS(3)
            WRITE (NOUT,*)
            WRITE (NOUT,*)
     +     '      Spectrum       Spectrum       Spectrum       Spectrum'
            WRITE (NOUT,*)
     +     '      estimate       estimate       estimate       estimate'
            WRITE (NOUT,99995) (I,XG(I),I=1,NG)
         END IF
      END IF
      STOP
*
99999 FORMAT (1X,A,I3)
99998 FORMAT (1X,6F11.4)
99997 FORMAT (1X,A,F4.1,A,F7.4)
99996 FORMAT (1X,A,F7.4,A,F7.4)
99995 FORMAT (1X,I4,F10.4,I5,F10.4,I5,F10.4,I5,F10.4)
      END
