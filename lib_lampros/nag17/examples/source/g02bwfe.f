*     G02BWF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          LDX, MMAX, MP
      PARAMETER        (LDX=12,MMAX=12,MP=(MMAX*(MMAX+1))/2)
*     .. Local Scalars ..
      DOUBLE PRECISION SW
      INTEGER          IFAIL, J, K, M, N
      CHARACTER        MEAN, WEIGHT
*     .. Local Arrays ..
      DOUBLE PRECISION C(MP), WMEAN(MMAX), WT(LDX), X(LDX,MMAX)
*     .. External Subroutines ..
      EXTERNAL         G02BUF, G02BWF, X04CCF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G02BWF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*,END=20) MEAN, WEIGHT, M, N
      IF (M.LE.MMAX .AND. N.LE.LDX) THEN
         READ (NIN,*) (WT(J),J=1,N)
         READ (NIN,*) ((X(J,K),K=1,M),J=1,N)
         IFAIL = 0
*
*        Calculate the sums of squares and cross-products matrix
         CALL G02BUF(MEAN,WEIGHT,N,M,X,LDX,WT,SW,WMEAN,C,IFAIL)
*
         IFAIL = -1
*
*        Calculate the correlation matrix
         CALL G02BWF(M,C,IFAIL)
*
*        Print the correlation matrix
         IF (IFAIL.EQ.0) THEN
            WRITE (NOUT,*)
            CALL X04CCF('Upper','Non-unit',M,C,'Correlation matrix',
     +                  IFAIL)
         ELSE IF (IFAIL.EQ.2) THEN
            WRITE (NOUT,*)
            WRITE (NOUT,*) ' NOTE: some variances are zero'
            WRITE (NOUT,*)
            CALL X04CCF('Upper','Non-unit',M,C,'Correlation matrix',
     +                  IFAIL)
         END IF
      ELSE
         WRITE (NOUT,99999) 'M or N is too large. M =', M, ', N =', N
      END IF
   20 STOP
*
99999 FORMAT (1X,A,I6,A,I6)
      END
