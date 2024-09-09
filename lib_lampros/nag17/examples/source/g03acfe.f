*     G03ACF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, MMAX, IWKMAX
      PARAMETER        (NMAX=9,MMAX=3,IWKMAX=50)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION TOL
      INTEGER          I, IFAIL, IRX, J, M, N, NCV, NG, NX
      CHARACTER        WEIGHT
*     .. Local Arrays ..
      DOUBLE PRECISION CVM(MMAX,MMAX), CVX(MMAX,MMAX), E(MMAX,6),
     +                 WK(IWKMAX), WT(NMAX), X(NMAX,MMAX)
      INTEGER          ING(NMAX), ISX(2*MMAX), NIG(MMAX)
*     .. External Subroutines ..
      EXTERNAL         G03ACF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G03ACF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N, M, NX, NG, WEIGHT
      IF (N.LE.NMAX .AND. M.LE.MMAX) THEN
         IF (WEIGHT.EQ.'W' .OR. WEIGHT.EQ.'w') THEN
            DO 20 I = 1, N
               READ (NIN,*) (X(I,J),J=1,M), WT(I), ING(I)
   20       CONTINUE
         ELSE
            DO 40 I = 1, N
               READ (NIN,*) (X(I,J),J=1,M), ING(I)
   40       CONTINUE
         END IF
         READ (5,*) (ISX(J),J=1,M)
         TOL = 0.000001D0
         IFAIL = 0
*
         CALL G03ACF(WEIGHT,N,M,X,NMAX,ISX,NX,ING,NG,WT,NIG,CVM,MMAX,E,
     +               MMAX,NCV,CVX,MMAX,TOL,IRX,WK,IWKMAX,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,99999) 'Rank of X = ', IRX
         WRITE (NOUT,*)
         WRITE (NOUT,*)
     +   'Canonical    Eigenvalues Percentage     CHISQ      DF     SIG'
         WRITE (NOUT,*) 'Correlations              Variation'
         DO 60 I = 1, NCV
            WRITE (NOUT,99998) (E(I,J),J=1,6)
   60    CONTINUE
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Canonical Coefficients for X'
         DO 80 I = 1, NX
            WRITE (NOUT,99997) (CVX(I,J),J=1,NCV)
   80    CONTINUE
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Canonical variate means'
         DO 100 I = 1, NG
            WRITE (NOUT,99997) (CVM(I,J),J=1,NCV)
  100    CONTINUE
      END IF
      STOP
*
99999 FORMAT (1X,A,I2)
99998 FORMAT (1X,2F12.4,F11.4,F10.4,F8.1,F8.4)
99997 FORMAT (1X,5F9.4)
      END
