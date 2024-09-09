*     G02BUF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          LDX, MMAX, MP
      PARAMETER        (LDX=12,MMAX=12,MP=(MMAX*(MMAX+1))/2)
      DOUBLE PRECISION ONE
      PARAMETER        (ONE=1.0D0)
*     .. Local Scalars ..
      DOUBLE PRECISION ALPHA, SW
      INTEGER          IFAIL, J, K, M, MM, N
      CHARACTER        MEAN, WEIGHT
*     .. Local Arrays ..
      DOUBLE PRECISION C(MP), V(MP), WMEAN(MMAX), WT(LDX), X(LDX,MMAX)
*     .. External Subroutines ..
      EXTERNAL         F06FDF, G02BUF, X04CCF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G02BUF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*,END=20) MEAN, WEIGHT, M, N
      IF (M.LE.MMAX .AND. N.LE.LDX) THEN
         READ (NIN,*) (WT(J),J=1,N)
         READ (NIN,*) ((X(J,K),K=1,M),J=1,N)
         IFAIL = 0
*
*        Calculate sums of squares and cross-products matrix
         CALL G02BUF(MEAN,WEIGHT,N,M,X,LDX,WT,SW,WMEAN,C,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Means'
         WRITE (NOUT,99999) (WMEAN(J),J=1,M)
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Weights'
         WRITE (NOUT,99999) (WT(J),J=1,N)
         WRITE (NOUT,*)
*        Print the sums of squares and cross products matrix
         CALL X04CCF('Upper','Non-unit',M,C,
     +               'Sums of squares and cross-products',IFAIL)
         IF (SW.GT.ONE) THEN
*           Calculate the variance matrix
            ALPHA = ONE/(SW-ONE)
            MM = (M*(M+1))/2
            CALL F06FDF(MM,ALPHA,C,1,V,1)
*           Print the variance matrix
            WRITE (NOUT,*)
            CALL X04CCF('Upper','Non-unit',M,V,'Variance matrix',IFAIL)
         END IF
      ELSE
         WRITE (NOUT,99998) 'M or N is too large. M =', M, ', N =', N
      END IF
   20 STOP
*
99999 FORMAT (1X,6F14.4)
99998 FORMAT (1X,A,I6,A,I6)
      END
