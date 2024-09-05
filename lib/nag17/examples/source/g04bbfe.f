*     G04BBF Example Program Text
*     Mark 16 Release. NAG Copyright 1992.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, NTMAX, NBMAX, TMAX
      PARAMETER        (NMAX=30,NTMAX=6,NBMAX=10,TMAX=4)
*     .. Local Scalars ..
      DOUBLE PRECISION GMEAN, TOL
      INTEGER          I, IFAIL, IRDF, J, N, NBLOCK, NT
*     .. Local Arrays ..
      DOUBLE PRECISION BMEAN(NBMAX), C(NTMAX,NTMAX), EF(NTMAX), R(NMAX),
     +                 TABLE(TMAX,5), TMEAN(NTMAX),
     +                 WK(NTMAX*NTMAX+NTMAX), Y(NMAX)
      INTEGER          IREP(NTMAX), IT(NMAX)
*     .. External Subroutines ..
      EXTERNAL         G04BBF
*     .. Executable Statements ..
      WRITE (NOUT,FMT=*) 'G04BBF Example Program Results'
*     Skip heading in data file
      READ (NIN,FMT=*)
      READ (NIN,FMT=*) N, NT, NBLOCK
      IF (N.LE.NMAX) THEN
         READ (NIN,FMT=*) (Y(I),I=1,N)
         READ (NIN,FMT=*) (IT(I),I=1,N)
         TOL = 0.000005D0
         IRDF = 0
         IFAIL = -1
*
         CALL G04BBF(N,Y,NBLOCK,NT,IT,GMEAN,BMEAN,TMEAN,TABLE,TMAX,C,
     +               NTMAX,IREP,R,EF,TOL,IRDF,WK,IFAIL)
*
         WRITE (NOUT,FMT=*)
         WRITE (NOUT,FMT=*) ' ANOVA table'
         WRITE (NOUT,FMT=*)
         WRITE (NOUT,FMT=*)
     +     '  Source        df         SS          MS          F',
     +     '        Prob'
         WRITE (NOUT,FMT=*)
         WRITE (NOUT,FMT=99998) ' Blocks      ', (TABLE(1,J),J=1,5)
         WRITE (NOUT,FMT=99998) ' Treatments  ', (TABLE(2,J),J=1,5)
         WRITE (NOUT,FMT=99998) ' Residual    ', (TABLE(3,J),J=1,3)
         WRITE (NOUT,FMT=99998) ' Total       ', (TABLE(4,J),J=1,2)
         WRITE (NOUT,FMT=*)
         WRITE (NOUT,FMT=*) ' Efficiency Factors'
         WRITE (NOUT,FMT=*)
         WRITE (NOUT,FMT=99999) (EF(I),I=1,NT)
         WRITE (NOUT,FMT=*)
         WRITE (NOUT,FMT=99997) '  Grand Mean', GMEAN
         WRITE (NOUT,FMT=*)
         WRITE (NOUT,FMT=*) ' Treatment Means'
         WRITE (NOUT,FMT=*)
         WRITE (NOUT,FMT=99999) (TMEAN(I),I=1,NT)
         WRITE (NOUT,FMT=*)
         WRITE (NOUT,FMT=*)
     +     ' Standard errors of differences between means'
         WRITE (NOUT,FMT=*)
         DO 20 I = 2, NT
            WRITE (NOUT,FMT=99999) (C(I,J),J=1,I-1)
   20    CONTINUE
      END IF
      STOP
*
99999 FORMAT (8F10.2)
99998 FORMAT (A,3X,F3.0,2X,3(F10.2,2X),F9.4)
99997 FORMAT (A,F10.2)
      END
