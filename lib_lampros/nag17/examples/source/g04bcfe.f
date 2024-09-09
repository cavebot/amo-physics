*     G04BCF Example Program Text
*     Mark 17 Release. NAG Copyright 1995.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, NTMAX, NBMAX
      PARAMETER        (NMAX=25,NTMAX=5,NBMAX=5)
*     .. Local Scalars ..
      DOUBLE PRECISION GMEAN
      INTEGER          I, IFAIL, J, N, NCOL, NREP, NROW, NT
*     .. Local Arrays ..
      DOUBLE PRECISION C(NTMAX,NTMAX), CMEAN(NBMAX), EF(NTMAX), R(NMAX),
     +                 RMEAN(NBMAX), RPMEAN(NBMAX), TABLE(6,5),
     +                 TMEAN(NTMAX), WK(3*NTMAX), Y(NMAX)
      INTEGER          IREP(NTMAX), IT(NMAX)
*     .. External Subroutines ..
      EXTERNAL         G04BCF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G04BCF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) NREP, NROW, NCOL, NT
      IF (NROW.LE.NBMAX .AND. NCOL.LE.NBMAX .AND. NT.LE.NTMAX) THEN
         N = NREP*NROW*NCOL
         READ (NIN,*) (Y(I),I=1,N)
         READ (NIN,*) (IT(I),I=1,N)
         IFAIL = -1
*
         CALL G04BCF(NREP,NROW,NCOL,Y,NT,IT,GMEAN,TMEAN,TABLE,6,C,NTMAX,
     +               IREP,RPMEAN,RMEAN,CMEAN,R,EF,0.00001D0,0,WK,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) ' ANOVA TABLE'
         WRITE (NOUT,*)
         IF (NREP.GT.1) THEN
            WRITE (NOUT,99998) ' Reps        ', (TABLE(1,J),J=1,5)
         END IF
         WRITE (NOUT,99998) ' Rows        ', (TABLE(2,J),J=1,5)
         WRITE (NOUT,99998) ' Columns     ', (TABLE(3,J),J=1,5)
         WRITE (NOUT,99998) ' Treatments  ', (TABLE(4,J),J=1,5)
         WRITE (NOUT,99998) ' Residual    ', (TABLE(5,J),J=1,3)
         WRITE (NOUT,99998) ' Total       ', (TABLE(NOUT,J),J=1,2)
         WRITE (NOUT,*)
         WRITE (NOUT,*) ' Treatment means'
         WRITE (NOUT,*)
         WRITE (NOUT,99999) (TMEAN(I),I=1,NT)
         WRITE (NOUT,*)
         WRITE (NOUT,99997)
     +     ' S.E. of difference (orthogonal design) = ', C(2,1)
      END IF
      STOP
*
99999 FORMAT (8F10.4)
99998 FORMAT (A,F3.0,2X,3(F10.4,2X),F8.4)
99997 FORMAT (A,F10.4)
      END
