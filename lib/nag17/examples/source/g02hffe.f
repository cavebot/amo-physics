*     G02HFF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, MMAX
      PARAMETER        (NMAX=5,MMAX=3)
*     .. Local Scalars ..
      DOUBLE PRECISION SIGMA
      INTEGER          I, IC, IFAIL, INDC, INDW, IX, J, K, M, N
*     .. Local Arrays ..
      DOUBLE PRECISION C(MMAX,MMAX), RS(NMAX), WGT(NMAX),
     +                 WK(MMAX*(NMAX+MMAX+1)+2*NMAX), X(NMAX,MMAX)
*     .. External Functions ..
      DOUBLE PRECISION PSI, PSP
      EXTERNAL         PSI, PSP
*     .. External Subroutines ..
      EXTERNAL         G02HFF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G02HFF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
*     Read in the dimensions of X
      READ (NIN,*) N, M
      WRITE (NOUT,*)
      IF (N.GT.0 .AND. N.LE.NMAX .AND. M.GT.0 .AND. M.LE.MMAX) THEN
*        Read in the X matrix
         DO 20 I = 1, N
            READ (NIN,*) (X(I,J),J=1,M)
   20    CONTINUE
*        Read in SIGMA
         READ (NIN,*) SIGMA
*        Read in weights and residuals
         DO 40 I = 1, N
            READ (NIN,*) WGT(I), RS(I)
   40    CONTINUE
*        Set other parameter values
         IX = NMAX
         IC = MMAX
*        Set parameters for Schweppe type regression
         INDW = 1
         INDC = 1
         IFAIL = 0
*
         CALL G02HFF(PSI,PSP,INDW,INDC,SIGMA,N,M,X,IX,RS,WGT,C,IC,WK,
     +               IFAIL)
*
         WRITE (NOUT,*) 'Covariance matrix'
         DO 60 J = 1, M
            WRITE (NOUT,99999) (C(J,K),K=1,M)
   60    CONTINUE
      END IF
      STOP
*
99999 FORMAT (1X,6F10.4)
      END
*
      DOUBLE PRECISION FUNCTION PSI(T)
*     .. Parameters ..
      DOUBLE PRECISION              C
      PARAMETER                     (C=1.5D0)
*     .. Scalar Arguments ..
      DOUBLE PRECISION              T
*     .. Intrinsic Functions ..
      INTRINSIC                     ABS
*     .. Executable Statements ..
      IF (T.LE.-C) THEN
         PSI = -C
      ELSE IF (ABS(T).LT.C) THEN
         PSI = T
      ELSE
         PSI = C
      END IF
      RETURN
      END
*
      DOUBLE PRECISION FUNCTION PSP(T)
*     .. Parameters ..
      DOUBLE PRECISION              C
      PARAMETER                     (C=1.5D0)
*     .. Scalar Arguments ..
      DOUBLE PRECISION              T
*     .. Intrinsic Functions ..
      INTRINSIC                     ABS
*     .. Executable Statements ..
      PSP = 0.0D0
      IF (ABS(T).LT.C) PSP = 1.0D0
      RETURN
      END
