*     G04DBF Example Program Text
*     Mark 17 Release. NAG Copyright 1995.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, NTMAX, NBMAX
      PARAMETER        (NMAX=26,NTMAX=4,NBMAX=1)
*     .. Local Scalars ..
      DOUBLE PRECISION CLEVEL, GMEAN, RDF, TOL
      INTEGER          I, IFAIL, IJ, IRDF, J, N, NBLOCK, NT
      CHARACTER        TYPE
*     .. Local Arrays ..
      DOUBLE PRECISION BMEAN(NBMAX), C(NTMAX,NTMAX),
     +                 CIL(NTMAX*(NTMAX-1)/2), CIU(NTMAX*(NTMAX-1)/2),
     +                 EF(NTMAX), R(NMAX), TABLE(4,5), TMEAN(NTMAX),
     +                 WK(NTMAX*NTMAX+NTMAX), Y(NMAX)
      INTEGER          IREP(NTMAX), ISIG(NTMAX*(NTMAX-1)/2), IT(NMAX)
      CHARACTER        STAR(2)
*     .. External Subroutines ..
      EXTERNAL         G04BBF, G04DBF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G04DBF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N, NT
      IF (N.LE.NMAX .AND. NT.LE.NTMAX) THEN
         READ (NIN,*) (Y(I),I=1,N)
         READ (NIN,*) (IT(I),I=1,N)
         TOL = 0.000005D0
         IRDF = 0
         NBLOCK = 1
         IFAIL = -1
         CALL G04BBF(N,Y,NBLOCK,NT,IT,GMEAN,BMEAN,TMEAN,TABLE,4,C,NTMAX,
     +               IREP,R,EF,TOL,IRDF,WK,IFAIL)
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
         WRITE (NOUT,*)
         WRITE (NOUT,*) ' Treatment means'
         WRITE (NOUT,*)
         WRITE (NOUT,99999) (TMEAN(J),J=1,NT)
         WRITE (NOUT,*)
         WRITE (NOUT,*) ' Simultaneous Confidence Intervals'
         WRITE (NOUT,*)
         RDF = TABLE(3,1)
         READ (NIN,*) TYPE, CLEVEL
*
         CALL G04DBF(TYPE,NT,TMEAN,RDF,C,NTMAX,CLEVEL,CIL,CIU,ISIG,
     +               IFAIL)
*
         STAR(2) = '*'
         STAR(1) = ' '
         IJ = 0
         DO 40 I = 1, NT
            DO 20 J = 1, I - 1
               IJ = IJ + 1
               WRITE (NOUT,99997) I, J, CIL(IJ), CIU(IJ),
     +           STAR(ISIG(IJ)+1)
   20       CONTINUE
   40    CONTINUE
      END IF
      STOP
*
99999 FORMAT (10F8.3)
99998 FORMAT (A,3X,F3.0,2X,2(F10.1,2X),F10.3,2X,F9.4)
99997 FORMAT (2X,2I2,3X,2(F10.3,3X),A)
      END
