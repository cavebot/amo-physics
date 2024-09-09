*     G04DAF Example Program Text
*     Mark 17 Release. NAG Copyright 1995.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, NTMAX, NBMAX
      PARAMETER        (NMAX=32,NTMAX=7,NBMAX=1)
*     .. Local Scalars ..
      DOUBLE PRECISION GMEAN, RDF, RMS, TOL
      INTEGER          I, IFAIL, IRDF, J, LDT, N, NBLOCK, NC, NT
*     .. Local Arrays ..
      DOUBLE PRECISION BMEAN(NBMAX), C(NTMAX,NTMAX), CT(NTMAX,NTMAX),
     +                 EF(NTMAX), EST(NTMAX), R(NMAX), TABLE(NTMAX+4,5),
     +                 TMEAN(NTMAX), TX(NTMAX), WK(NTMAX*NTMAX+NTMAX),
     +                 Y(NMAX)
      INTEGER          IREP(NTMAX), IT(NMAX)
      CHARACTER*11     NAMES(NTMAX)
*     .. External Subroutines ..
      EXTERNAL         G04BBF, G04DAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G04DAF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N, NT
      IF (N.LE.NMAX .AND. NT.LE.NTMAX) THEN
         READ (NIN,*) (Y(I),I=1,N)
         READ (NIN,*) (IT(I),I=1,N)
         TOL = 0.000005D0
         IRDF = 0
         NBLOCK = 1
         LDT = NTMAX + 4
         IFAIL = -1
         CALL G04BBF(N,Y,NBLOCK,NT,IT,GMEAN,BMEAN,TMEAN,TABLE,LDT,C,
     +               NTMAX,IREP,R,EF,TOL,IRDF,WK,IFAIL)
         WRITE (NOUT,*)
         WRITE (NOUT,*) ' ANOVA table'
         WRITE (NOUT,*)
         WRITE (NOUT,*)
     +     '  Source        df         SS          MS          F',
     +     '        Prob'
         WRITE (NOUT,*)
         WRITE (NOUT,99998) ' Treatments', (TABLE(2,J),J=1,5)
         WRITE (NOUT,99998) ' Residual  ', (TABLE(3,J),J=1,3)
         WRITE (NOUT,99998) ' Total     ', (TABLE(4,J),J=1,2)
*
         RMS = TABLE(3,3)
         RDF = TABLE(3,1)
         READ (NIN,*) NC
         WRITE (NOUT,*)
         WRITE (NOUT,*) ' Orthogonal Contrasts'
         WRITE (NOUT,*)
         DO 20 I = 1, NC
            READ (NIN,*) (CT(J,I),J=1,NT)
            READ (NIN,99999) NAMES(I)
   20    CONTINUE
         CALL G04DAF(NT,TMEAN,IREP,RMS,RDF,NC,CT,NTMAX,EST,TABLE(5,1),
     +               LDT,TOL,.FALSE.,TX,IFAIL)
         DO 40 I = 1, NC
            WRITE (NOUT,99998) NAMES(I), (TABLE(I+4,J),J=1,5)
   40    CONTINUE
      END IF
      STOP
*
99999 FORMAT (A)
99998 FORMAT (A,3X,F3.0,2X,2(F10.1,2X),F10.3,2X,F9.4)
      END
