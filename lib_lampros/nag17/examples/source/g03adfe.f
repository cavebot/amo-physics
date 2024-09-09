*     G03ADF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, IMAX, IWKMAX
      PARAMETER        (NMAX=9,IMAX=2,IWKMAX=40)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION TOL
      INTEGER          I, IFAIL, IX, IY, J, M, N, NCV, NX, NY
      CHARACTER        WEIGHT
*     .. Local Arrays ..
      DOUBLE PRECISION CVX(IMAX,IMAX), CVY(IMAX,IMAX), E(IMAX,6),
     +                 WK(IWKMAX), WT(NMAX), Z(NMAX,2*IMAX)
      INTEGER          ISZ(2*IMAX)
*     .. External Subroutines ..
      EXTERNAL         G03ADF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G03ADF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N, M, IX, IY, WEIGHT
      IF (N.LE.NMAX .AND. IX.LE.IMAX .AND. IY.LE.IMAX) THEN
         IF (WEIGHT.EQ.'W' .OR. WEIGHT.EQ.'w') THEN
            DO 20 I = 1, N
               READ (NIN,*) (Z(I,J),J=1,M), WT(I)
   20       CONTINUE
         ELSE
            DO 40 I = 1, N
               READ (NIN,*) (Z(I,J),J=1,M)
   40       CONTINUE
         END IF
         READ (5,*) (ISZ(J),J=1,M)
         TOL = 0.000001D0
         NX = IX
         NY = IY
         IFAIL = 0
*
         CALL G03ADF(WEIGHT,N,M,Z,NMAX,ISZ,NX,NY,WT,E,IMAX,NCV,CVX,IMAX,
     +               IMAX,CVY,IMAX,TOL,WK,IWKMAX,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,99999) 'Rank of X = ', NX, ' Rank of Y = ', NY
         WRITE (NOUT,*)
         WRITE (NOUT,*)
     +   'Canonical    Eigenvalues Percentage     Chisq      DF     Sig'
         WRITE (NOUT,*) 'correlations              variation'
         DO 60 I = 1, NCV
            WRITE (NOUT,99998) (E(I,J),J=1,6)
   60    CONTINUE
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Canonical coefficients for X'
         DO 80 I = 1, IX
            WRITE (NOUT,99997) (CVX(I,J),J=1,NCV)
   80    CONTINUE
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Canonical coefficients for Y'
         DO 100 I = 1, IY
            WRITE (NOUT,99997) (CVY(I,J),J=1,NCV)
  100    CONTINUE
      END IF
      STOP
*
99999 FORMAT (1X,A,I2,A,I2)
99998 FORMAT (1X,2F12.4,F11.4,F10.4,F8.1,F8.4)
99997 FORMAT (1X,5F9.4)
      END
