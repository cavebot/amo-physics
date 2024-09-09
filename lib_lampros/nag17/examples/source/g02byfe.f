*     G02BYF Example Program Text
*     Mark 17 Release. NAG Copyright 1995.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          LDX, MMAX, LDV
      PARAMETER        (LDX=20,MMAX=10,LDV=MMAX)
*     .. Local Scalars ..
      INTEGER          IFAIL, J, K, M, N, NX, NY
*     .. Local Arrays ..
      DOUBLE PRECISION R(LDV,MMAX), STD(MMAX), V(LDV,MMAX),
     +                 WK(MMAX*MMAX), WT(LDX), X(LDX,MMAX), XBAR(MMAX)
      INTEGER          ISZ(MMAX)
*     .. External Subroutines ..
      EXTERNAL         G02BXF, G02BYF, X04CAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G02BYF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N, M
      IF (M.LE.MMAX .AND. N.LE.LDX) THEN
         READ (NIN,*) ((X(J,K),K=1,M),J=1,N)
*
*        Calculate correlation matrix
*
         IFAIL = -1
*
         CALL G02BXF('U',N,M,X,LDX,WT,XBAR,STD,V,LDV,R,IFAIL)
*
         IF (IFAIL.EQ.0) THEN
*
*        Print the correlation matrix
*
            WRITE (NOUT,*)
            CALL X04CAF('Upper','Non-unit',M,M,R,LDV,
     +                  'Correlation matrix',IFAIL)
            READ (NIN,*) NY, NX
            READ (NIN,*) (ISZ(J),J=1,M)
*
*           Calculate partial correlation matrix
*
            IFAIL = 0
*
            CALL G02BYF(M,NY,NX,ISZ,V,LDV,R,LDV,WK,IFAIL)
*
*           Print partial correlation matrix
*
            WRITE (NOUT,*)
            CALL X04CAF('Upper','Unit',NY,NY,R,LDV,
     +                  'Partial Correlation matrix',IFAIL)
         END IF
      END IF
      STOP
      END
