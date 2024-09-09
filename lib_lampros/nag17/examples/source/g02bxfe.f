*     G02BXF Example Program Text
*     Mark 17 Revised.  NAG Copyright 1995.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          MMAX, NMAX
      PARAMETER        (NMAX=20,MMAX=5)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J, K, LDX, LDV, M, N
      CHARACTER        WEIGHT
*     .. Local Arrays ..
      DOUBLE PRECISION R(MMAX,MMAX), STD(MMAX), V(MMAX,MMAX), WT(NMAX),
     +                 X(NMAX,MMAX), XBAR(MMAX)
*     .. External Subroutines ..
      EXTERNAL         G02BXF, X04CAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G02BXF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) WEIGHT, N, M
      IF (M.LE.MMAX .AND. N.LE.NMAX) THEN
         IF (WEIGHT.EQ.'W' .OR. WEIGHT.EQ.'w') THEN
            DO 20 I = 1, N
               READ (NIN,*) (X(I,J),J=1,M), WT(I)
 20         CONTINUE
         ELSE
            DO 40 I = 1, N
               READ (NIN,*) (X(I,J),J=1,M)
 40         CONTINUE
         END IF
         LDX = NMAX
         LDV = MMAX
         IFAIL = -1
*
         CALL G02BXF(WEIGHT,N,M,X,LDX,WT,XBAR,STD,V,LDV,R,IFAIL)
*
         IF (IFAIL.EQ.0 .OR. IFAIL.EQ.5) THEN
            WRITE (NOUT,*)
            WRITE (NOUT,*) '     Means'
            WRITE (NOUT,*)
            WRITE (NOUT,99999) (XBAR(I),I=1,M)
            WRITE (NOUT,*)
            WRITE (NOUT,*) '     Standard deviations'
            WRITE (NOUT,*)
            WRITE (NOUT,99999) (STD(I),I=1,M)
*
*           Print the correlation matrix
*
            IF (IFAIL.EQ.5) THEN
               WRITE (NOUT,*)
               WRITE (NOUT,*) ' NOTE: some variances are zero'
            END IF
            WRITE (NOUT,*)
            CALL X04CAF('Upper','Non-unit',M,M,R,LDV,
     +                     '     Correlation matrix',IFAIL)
            WRITE (NOUT,*)
         END IF
      ELSE
         WRITE (NOUT,99998) 'M or N is too large. M =', M, ', N =', N
      END IF
      STOP
*
99999 FORMAT (1X,10F13.4)
99998 FORMAT (1X,A,I6,A,I6)
      END
