*     G13CGF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NGMAX, LMAX
      PARAMETER        (NGMAX=9,LMAX=16)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION ERLW, ERUP, RFSE
      INTEGER          I, IFAIL, J, L, N, NG
*     .. Local Arrays ..
      DOUBLE PRECISION ER(NGMAX), RF(LMAX), STATS(4), XG(NGMAX),
     +                 XYIG(NGMAX), XYRG(NGMAX), YG(NGMAX)
*     .. External Subroutines ..
      EXTERNAL         G13CGF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G13CGF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) NG, L, N
      IF (NG.GT.0 .AND. NG.LE.NGMAX .AND. L.GT.0 .AND. L.LE.LMAX) THEN
         READ (NIN,*) (STATS(I),I=1,4)
         READ (NIN,*) (XG(I),YG(I),XYRG(I),XYIG(I),I=1,NG)
         IFAIL = 1
*
         CALL G13CGF(XG,YG,XYRG,XYIG,NG,STATS,L,N,ER,ERLW,ERUP,RF,RFSE,
     +               IFAIL)
*
         WRITE (NOUT,*)
         IF (IFAIL.NE.0) THEN
            WRITE (NOUT,99999) 'G13CGF fails. IFAIL =', IFAIL
            WRITE (NOUT,*)
         END IF
         IF (IFAIL.NE.1) THEN
            WRITE (NOUT,*) '           Noise spectrum'
            DO 20 J = 1, NG
               WRITE (NOUT,99998) J - 1, ER(J)
   20       CONTINUE
            WRITE (NOUT,*)
            WRITE (NOUT,*) 'Noise spectrum bounds multiplying factors'
            WRITE (NOUT,99997) 'Lower =', ERLW, '     Upper =', ERUP
            WRITE (NOUT,*)
            WRITE (NOUT,*) 'Impulse response function'
            WRITE (NOUT,*)
            DO 40 J = 1, L
               WRITE (NOUT,99998) J - 1, RF(J)
   40       CONTINUE
            WRITE (NOUT,*)
            WRITE (NOUT,99997)
     +        'Impulse response function standard error =', RFSE
         END IF
      END IF
      STOP
*
99999 FORMAT (1X,A,I3)
99998 FORMAT (1X,I5,F16.4)
99997 FORMAT (1X,A,F10.4,A,F10.4)
      END
